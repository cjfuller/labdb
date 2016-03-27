require 'fileutils'
require 'yaml'

INJECT_VERSION_REPLACE = "<VERSION>"
DOCKER_MACHINE_NAME = "dev"
DOCKER_REGISTRY_NAME = "us.gcr.io"
PROJECT_NAME = "straightlabdb"
CONTAINER_NAME = "labdb_web"
CONTROLLER_NAME = "labdb-web"


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
  system(*args) || exit(1)
end

def container_name(version)
  "#{CONTAINER_PREFIX}:#{version}"
end

def docker_env
  "eval $(docker-machine env #{DOCKER_MACHINE_NAME}) &&"
end

def docker_build(version)
  cmd("#{docker_env} docker build -t #{container_name(version)} .")
  cmd("#{docker_env} gcloud --project #{PROJECT_NAME} docker push #{container_name(version)}")
end

def interpolate_version(fn, version)
  new_fn = fn.gsub(".yaml", "_#{version}.yaml")
  content = File.read(fn)
  puts content
  new_content = content.gsub(INJECT_VERSION_REPLACE, version)
  IO.write(new_fn, new_content)
  new_fn
end

def update_image(version)
  UPDATE_STAGES.each do |f|
    new_fn = interpolate_version(f, version)
    if new_fn.include? "controller" then
      name = YAML.load(File.read(new_fn))['metadata']['name']
      cmd("kubectl rolling-update #{name} --image=#{container_name(version)}")
    end
    FileUtils.rm(new_fn)
  end
end

def cmd_update
  puts "Deploying change from #{prev_version} to #{NEXT_VERSION}."
  docker_build(NEXT_VERSION)
  update_image(NEXT_VERSION)
end

cmd_update
