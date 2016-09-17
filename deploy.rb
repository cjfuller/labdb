#!/usr/bin/env ruby

require 'fileutils'
require 'optparse'
require 'yaml'

UPDATE_STAGES = [
  "./labdb_web_deployment.yaml"
]
DEPLOY_STAGES = [
  "./postgres_controller.yaml",
  "./postgres_service.yaml",
  "./labdb_web_deployment.yaml",
  "./labdb_web_service.yaml",
]
INJECT_VERSION_REPLACE = "<VERSION>"
INJECT_LAB_REPLACE = "<LAB>"
DOCKER_MACHINE_NAME = "dev"
DOCKER_REGISTRY_NAME = "us.gcr.io"
PROJECT_NAME = "labdb-io"
CONTAINER_NAME = "labdb_web"

LAB_TEMPLATES = [
  "./labdb_web_deployment.yaml",
  "./labdb_web_service.yaml",
  "./postgres_controller.yaml",
  "./postgres_service.yaml",
  "./config/database.yml"
]
TEMPLATE_SUFFIX = ".template"


PREVIOUS_VERSION = /labdb-web-([0-9a-f]*)/.match(`kubectl get services`)&.[](1)&.strip
# TODO: allow specifying a destination version.
NEXT_VERSION = `git log -n 1 --format=%h`.strip
CONTAINER_PREFIX = "#{DOCKER_REGISTRY_NAME}/#{PROJECT_NAME}/#{CONTAINER_NAME}"

def prev_version
  pods = `kubectl -o yaml get pods`
  pod_name = /#{CONTAINER_PREFIX}:([0-9a-f]+)/.match(pods)&.[](1)
  exit(1) unless pod_name
  pod_name
end

def cmd(*args)
  puts "Running: #{args}"
  system(*args) || exit(1)
end

def container_name(version)
  "#{CONTAINER_PREFIX}:#{version}"
end

def docker_env
  ""
  #"eval $(docker-machine env #{DOCKER_MACHINE_NAME}) && "
end

def before_deploy
  cmd("gcloud config set project #{PROJECT_NAME}")
end

def build_proxy()
  # TODO(colin): move this to Dockerfile or move all build steps here.
  cmd("sbt assembly")
end

def docker_build(version)
  cmd("#{docker_env}docker build -t #{container_name(version)} .")
  cmd("#{docker_env}gcloud --project #{PROJECT_NAME} docker push #{container_name(version)}")
end

def interpolate_version(fn, version)
  new_fn = fn.gsub(".yaml", "_#{version}.yaml")
  content = File.read(fn)
  puts content
  new_content = content.gsub(INJECT_VERSION_REPLACE, version)
  IO.write(new_fn, new_content)
  new_fn
end

def interpolate_template(lab, dest_file)
  File.write(dest_file,
             File.read(dest_file + TEMPLATE_SUFFIX)
               .gsub(INJECT_LAB_REPLACE, lab)
               .gsub(INJECT_VERSION_REPLACE, NEXT_VERSION)
            )
end

def interpolate_templates(lab)
  LAB_TEMPLATES.each do |lt|
    interpolate_template(lab, lt)
  end
end

def update_image(version)
  UPDATE_STAGES.each do |f|
    new_fn = interpolate_version(f, version)
    if new_fn.include? "deployment" then
      cmd("kubectl replace -f #{new_fn}")
    end
    FileUtils.rm(new_fn)
  end
end

def fresh_deploy
  DEPLOY_STAGES.each do |f|
    cmd("kubectl create -f #{f}")
  end
end

def cmd_update
  puts "Deploying change from #{prev_version} to #{NEXT_VERSION}."
  update_image(NEXT_VERSION)
end

def parse_opts
  options = {}

  OptionParser.new do |opts|
    opts.banner = "LabDB deployment script.  Usage: deploy.rb [options]"
    opts.on("--all", "Deploy an update to all labs") do |all|
      options[:all] = all
    end
    opts.on("--lab LAB", String, "Deploy an update to the specified lab.") do |lab|
      options[:lab] = lab
    end
    opts.on("--templates-only", "Only do template interpolation.") do |temp_only|
      options[:templates_only] = temp_only
    end
    opts.on("--fresh-deploy", "Deploy a fresh version from scratch, rather than updating.") do |deploy|
      options[:deploy] = deploy
    end
  end.parse!
  options
end


def main
  opts = parse_opts
  if opts[:all] then
    raise "--all not yet implemented"
  end
  if !opts[:lab] then
    raise "Must supply a lab with --lab"
  end
  interpolate_templates(opts[:lab])
  return if opts[:templates_only]
  before_deploy
  build_proxy
  docker_build(NEXT_VERSION)
  if opts[:deploy] then
    fresh_deploy
  else
    cmd_update
  end
end

if __FILE__ == $0 then
  main
end
