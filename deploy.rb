#!/usr/bin/env ruby
require 'colorize'
require 'trollop'

UPDATE_STAGES = [
  './labdb_web_deployment.yaml',
]

DEPLOY_STAGES = [
  './postgres_controller.yaml',
  './postgres_service.yaml',
  './labdb_web_deployment.yaml',
  './labdb_web_service.yaml',
]

INJECT_VERSION_REPLACE = '<VERSION>'
INJECT_LAB_REPLACE = '<LAB>'
DOCKER_REGISTRY_NAME = 'us.gcr.io'
PROJECT_NAME = 'labdb-io'
CONTAINER_NAME = 'labdb_web'

LAB_TEMPLATES = DEPLOY_STAGES + ['./config/database.yml']

TEMPLATE_SUFFIX = '.template'
NEXT_VERSION = `git log -n 1 --format=%h`.strip

CONTAINER_PREFIX = "#{DOCKER_REGISTRY_NAME}/#{PROJECT_NAME}/#{CONTAINER_NAME}"


def previous_version
  pods = `kubectl -o yaml get pods`
  re = /#{CONTAINER_PREFIX}:([0-9a-f]+)/m
  if matchobj = re.match(pods)
    matchobj[1]
  else
    nil
  end
end

def cmd(args)
  cmd_str = args.join(' ')
  puts "Running #{cmd_str}".light_white
  result = system cmd_str
  raise "#{cmd_str} existed with error" unless result
end

def container_name(version) "#{CONTAINER_PREFIX}:#{version}" end

def before_deploy(version)
  cmd ["gcloud", "config", "set", "project", PROJECT_NAME]
  cmd %w(npm install)
  cmd %w(npm run-script coffee-compile)
  cmd %w(npm run-script compile)
  cmd ["mv", "public/_s/app_.js", "public/_s/app_#{version}.js"]
  puts 'Writing version to config/version.txt'.light_white
  File.open('config/version.txt', 'w') do |f|
    f.print(version)
  end
  puts '-> OK'.green
end

def docker_build(version)
  cmd ["docker", "build", "-t", container_name(version), "."]
  cmd ["gcloud", "--project", PROJECT_NAME, "docker", "push", container_name(version)]
end

def interpolate_template(dest_filename, version, lab)
  source_filename = dest_filename + TEMPLATE_SUFFIX
  puts "interpolating #{source_filename} to #{dest_filename}"
  interpolated = File.read(source_filename)
    .gsub(INJECT_VERSION_REPLACE, version)
    .gsub(INJECT_LAB_REPLACE, lab)
  File.open(dest_filename, 'w') { |f| f.write(interpolated) }
end

def interpolate_templates(version, lab)
  LAB_TEMPLATES.each { |t| interpolate_template(t, version, lab) }
end

def do_update(version)
  UPDATE_STAGES.each { |f| cmd ['kubectl', 'replace', '-f', f] }
end

def main
  opts = Trollop::options do
    opt :lab, "Lab", type: :string
    opt :'templates-only'
  end
  Trollop::die :lab, "must be specified" unless opts[:lab]
  version = NEXT_VERSION
  interpolate_templates(version, opts[:lab])
  return if opts[:'templates-only']

  before_deploy(version)
  docker_build(version)
  do_update(version)
end

main if __FILE__ == $0
