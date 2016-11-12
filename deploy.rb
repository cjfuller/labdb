#!/usr/bin/env ruby
require 'colorize'
require 'trollop'

def cmd(args, ignore_err: false)
  cmd_str = args.join(' ')
  puts "Running #{cmd_str}".light_white
  result = system cmd_str
  raise "#{cmd_str} existed with error" unless result or ignore_err
end

def js_file(version)
  "app_#{version}.js"
end

def gcs_path(version)
  "gs://labdb-static/#{js_file(version)}"
end

def before_deploy(version)
  cmd ["heroku", "config:set", "SECRET_TOKEN=$(rake secret)"]
  cmd ["heroku", "config:set", "JS_VERSION=#{version}"]
  cmd ['rm', 'public/_s/*.js'], ignore_err: true
  cmd %w(npm install)
  cmd %w(npm run-script coffee-compile)
  cmd %w(npm run-script compile)
  cmd ['gsutil', 'cp', 'public/_s/app_.js', gcs_path(version)]
  cmd ['gsutil', 'acl', 'ch', '-u', 'AllUsers:R', gcs_path(version)]
  puts '-> OK'.green
end

def deploy(lab)
  cmd ['git', 'push', '-f', "heroku-#{lab}", 'HEAD:master']
end

def roll_back_local(lab)
  cmd ['git', 'reset', '--hard', "origin/#{lab}"]
end

def main
  opts = Trollop::options do
    opt :lab, "Lab", type: :string
  end
  Trollop::die :lab, "must be specified" unless opts[:lab]
  cmd ['git', 'checkout', opts[:lab]]
  version = `git log -1 --format=%h`.strip
  before_deploy(version)
  deploy(opts[:lab])
  roll_back_local(opts[:lab])
end

main if __FILE__ == $0
