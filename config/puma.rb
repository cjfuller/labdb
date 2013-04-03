#!/usr/bin/env puma

proj_base = File.expand_path("../..", __FILE__)

key_path = ""
cert_path = ""

puts proj_base


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
pidfile File.expand_path('tmp/pids/puma.pid', proj_base)

# Use “path” as the file to store the server info state. This is
# used by “pumactl” to query and control the server.
#
state_path File.expand_path('tmp/pids/puma.state', proj_base)

# Redirect STDOUT and STDERR to files specified. The 3rd parameter
# (“append”) specifies whether the output is appended, the default is
# “false”.
#
# stdout_redirect '/u/apps/lolcat/log/stdout', '/u/apps/lolcat/log/stderr'
#stdout_redirect File.expand_path('log/stdout', proj_base), File.expand_path('log/stderr', proj_base), true

# Bind the server to “url”. “tcp://”, “unix://” and “ssl://” are the only

bind 'tcp://127.0.0.1:3000'
#ssl_bind '127.0.0.1', '3000', { key: key_path, cert: cert_path }

# Command to use to restart puma. This should be just how to
# load puma itself (ie. 'ruby -Ilib bin/puma'), not the arguments
# to puma, as those are the same as the original process.
#
# restart_command '/u/app/lolcat/bin/restart_puma'
