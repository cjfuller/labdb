#!/usr/bin/env puma
# coding: utf-8

require 'fileutils'

proj_base = File.expand_path("../..", __FILE__)
pid_path = File.expand_path("tmp/pids", proj_base)
key_path = File.expand_path('config/gen_key.pem', proj_base)
cert_path = File.expand_path('config/gen_cert.pem', proj_base)

FileUtils.mkdir_p(pid_path)

# Set the environment in which the rack's app will run.
#
# The default is “development”.
#
# environment = :production

# Daemonize the server into the background. Highly suggest that
# this be combined with “pidfile” and “stdout_redirect”.
#
# The default is “false”.
#
# daemonize
# daemonize false

# Store the pid of the server in the file at “path”.
#
pidfile File.expand_path('puma.pid', pid_path)

# Use “path” as the file to store the server info state. This is
# used by “pumactl” to query and control the server.
#
state_path File.expand_path('puma.state', pid_path)

# Redirect STDOUT and STDERR to files specified. The 3rd parameter
# (“append”) specifies whether the output is appended, the default is
# “false”.
#
# stdout_redirect '/u/apps/lolcat/log/stdout', '/u/apps/lolcat/log/stderr'
#stdout_redirect File.expand_path('log/stdout', proj_base), File.expand_path('log/stderr', proj_base), true

# Bind the server to “url”. “tcp://”, “unix://” and “ssl://” are the only

bind 'tcp://0.0.0.0:3000'

# Command to use to restart puma. This should be just how to
# load puma itself (ie. 'ruby -Ilib bin/puma'), not the arguments
# to puma, as those are the same as the original process.
#
# restart_command '/u/app/lolcat/bin/restart_puma'
