#!/usr/bin/env python

"""
Various tasks related to installing/updating/managing the database.

Run manage.py help for a listing of tasks, or manage.py help taskname for more information about a specific task.

Why python, given that this is a ruby project?  Eventually this script will
allow updating the ruby interpreter and environment; I wanted to avoid a
script that could make changes that might break the script itself.
"""

import argparse
import datetime
import os, os.path
import stat
import subprocess
import sys
import random

#constants for the installation

COMMAND_PREFIX = "--> "
CONFIRM_PREFIX = "--? "

#git parameters
BR_DEPLOY_STAGING = 'deploy_staging'
BR_DEPLOY = 'deploy'
BR_REMOTE = 'master'
REM_NAME = 'origin'
PROJECT_URL = 'https://github.com/cjfuller/labdb.git'
DEFAULT_REPO_PATH = '~/labdb'
DEFAULT_BACKUP_DIR = '~/backups'
AUTO_MERGE_MESSAGE = '"auto merge by manage.py"'

#database backend binaries
PG_DUMP_PATH = subprocess.check_output(['which', 'pg_dump'])

#paths to various config files
HOSTNAME_CFG_FILE = 'config/full_hostname.txt'
SECRET_CFG_FILE = 'config/secret_token.txt'

#command confirmation parameter
SHOULD_CONFIRM = False

# input vs. raw_input hack; don't want people to waste time with version issues
try: 
    input = raw_input
except NameError:
    pass

#terminal coloring (manually doing this to avoid dependencies)
TERM_COLORS = { 'green' : '\033[92m',
                'yellow': '\033[93m',
                'red'   : '\033[91m',
                'off'   : '\033[0m',}

def color(cname, text):
    """Display text in the color cname.  Reset color after the print."""
    return TERM_COLORS[cname] + text + TERM_COLORS['off']

def print_command(cmd):
    """Print a command being run in standard format."""
    print(color('green', COMMAND_PREFIX + cmd))

#command confirmation
def ask_confirm(a_command):
    """Ask the user for confirmation of the specified command.

    If confirmation is not given, exits the program immediately without running the command.

    """
    print(color('yellow', CONFIRM_PREFIX + a_command))
    response = input(color('yellow', "OK? [y]/n: "))
    if response == 'n':
        print("Aborting.")
        exit(0)

def should_confirm():
    """Check whether confirmation has been requested on the command line."""
    return SHOULD_CONFIRM

def confirm(fct): 
    """
    Decorator for adding a confirmation step before returning from a command
    function.

    Note that the command function will be called whether or not confirmation
    is given (it should return a command string).

    """
    def newfct(*args, **kwargs):
        cmd = fct(*args, **kwargs)
        if should_confirm():
            ask_confirm(cmd)
        return cmd
    return newfct

shell_command_list = []

def queue_command(cmd, shell_command_list=shell_command_list):
    """Add a ShellCommand object to the queue for later processing."""
    shell_command_list.append(cmd)

def run_queued_commands(shell_command_list=shell_command_list):
    """Process and remove all ShellCommand objects in the queue."""
    for c in shell_command_list:
        c()
    shell_command_list = []

class ShellCommand(object):
    """A command that gets run in a login shell."""

    @staticmethod
    def prepend_login_shell(command):
        return ['/bin/bash', '--login', '-c', command]

    def __init__(self, cmd_fct, args=None, requires_sudo=False, exit_on_fail=True):
        self.cmd_fct = cmd_fct
        self.requires_sudo = requires_sudo
        self.exit_on_fail = exit_on_fail
        self.args = args

    def __call__(self):
        """Run the command using function call notation."""
        if self.args is None:
            command = self.cmd_fct()
        else:
            command = self.cmd_fct(*args)
        print_command(command)
        command = self.prepend_login_shell(command)
        retval = subprocess.call(command)
        if (retval > 0 and self.exit_on_fail):
            print(color('red', 'Encountered an unresolvable error while running `{0}`.  Please resolve the problem manually and re-run.'.format(command[-1])))
            exit(retval)
        return retval

    def __str__(self):
        return self.cmd_fct()

    def __unicode__(self):
        return self.cmd_fct()

# git commands

def check_for_staging_branch():
    """
    Check (via process return code) if the staging branch already exists.

    Return: 0 if it does exist, nonzero otherwise.

    """
    return 'git show-ref --verify --quiet refs/heads/{0}'.format(BR_DEPLOY_STAGING)

@confirm
def clean_up_staging_branch():
    """Delete the staging branch.  Don't check for its existence."""
    return "git checkout {0} && git branch -d {1}".format(BR_DEPLOY, BR_DEPLOY_STAGING)

def create_staging_branch():
    """Create the staging branch.  Don't check for its existence."""
    return "git checkout {0} && git branch {1}".format(BR_DEPLOY, BR_DEPLOY_STAGING)

def fetch_remote_changes():
    """
    Download changes from the central labdb repo set up as a remote.

    The remote name and branch to download are set up in the parameters above.
    """
    return "git checkout {0} && git pull {1} {0}".format(BR_REMOTE, REM_NAME)

def stage_changes():
    """
    Merge changes from the downloaded branch into the staging branch.

    This is where problems should arise if there are merge conflicts.  If this
    happens and you need to get back to a workable state, you can force the
    load of the (still unchanged) deploy branch using `manage.py revert-
    failure`.

    """
    return "git checkout {0} && git merge -m {1} {2}".format(
        BR_DEPLOY_STAGING, AUTO_MERGE_MESSAGE, BR_REMOTE)

def merge_into_production():
    """Merge changes from staging into production."""
    return "git checkout {0} && git merge -m {1} {2}".format(
        BR_DEPLOY, AUTO_MERGE_MESSAGE, BR_DEPLOY_STAGING)

def revert_merge_failure():
    """Revert a merge and go back to production in case conflicts arise."""
    return "git reset --merge && git checkout {0}".format(BR_DEPLOY)

# application commands

def update_deps_conservative():
    """Update dependencies as needed from Gemfile."""
    return "bundle install"

@confirm
def update_all_deps():
    """Update all dependencies to the latest versions that are compatible."""
    return "bundle update"

def precompile_assets():
    """Precompile assets (stylesheets/javascripts) for production."""
    return "bundle exec rake assets:precompile"

def create_production_db():
    """Create the (empty) database for the production server."""
    return "RAILS_ENV=production bundle exec rake db:setup"

# server status commands

def create_backup():
    """Backup the postgres database to a file."""
    suffix = "_labdb_backup.dump"
    backup_timestring = datetime.datetime.today().strftime("%Y%m%d_%H%M%S")
    fn = backup_timestring + suffix
    fn_full = os.path.join(DEFAULT_BACKUP_DIR, fn)
    return ' '.join([   PG_DUMP_PATH,
                        '-h localhost labdb > {0}'.format(fn_full),
                        '&&',
                        'tar cjf {0}.tar.bz2 -C {1} {2}'.format(fn_full,
                            DEFAULT_BACKUP_DIR, fn),
                        '&&',
                        'rm -f {0}'.format(fn_full)])

def restart_server():
    """Restart the webserver."""
    return "supervisorctl restart labdb"

#other commands not using the shell

def set_hostname(hostname=None):
    """
    Prompt for the hostname of the machine and write it to the appropriate
    config file.

    """
    if hostname is None:
        hostname = input('Please enter the full hostname of the machine.\n'
                '(i.e. the part that would appear including the https:// in a url but before any other slashes): ')
    with open(HOSTNAME_CFG_FILE, 'w') as f:
        f.write(hostname)

def generate_application_secret():
    """
    Generate an application secret for the application and write it to the
    appropriate config file.

    """
    print_command("Generating application secret")
    s_rng = random.SystemRandom()
    secret_string = '{0:x}'.format(s_rng.getrandbits(512))
    with open(SECRET_CFG_FILE, 'w') as f:
        f.write(secret_string)
    os.chmod(SECRET_CFG_FILE, stat.S_IRUSR | stat.S_IWUSR)


# task helpers

def ok():
    print(color('green', "OK"))

# tasks that can be run from the command line

def update():
    queue_command(ShellCommand(create_backup))
    if ShellCommand(check_for_staging_branch, exit_on_fail=False)() == 0:
        queue_command(ShellCommand(clean_up_staging_branch))
    queue_command(ShellCommand(create_staging_branch))
    queue_command(ShellCommand(fetch_remote_changes))
    queue_command(ShellCommand(stage_changes))
    queue_command(ShellCommand(update_deps_conservative))
    queue_command(ShellCommand(precompile_assets))
    queue_command(ShellCommand(merge_into_production))
    queue_command(ShellCommand(restart_server))

def backup():
    queue_command(ShellCommand(create_backup))

def force_update_deps():
    queue_command(ShellCommand(update_all_deps))

def secret():
    generate_application_secret()

def hostname(hn=None):
    set_hostname(hn)

def install():
    queue_command(ShellCommand(create_production_db))

def revert_failure():
    queue_command(ShellCommand(revert_merge_failure))

def restart():
    queue_command(ShellCommand(restart_server))

def help():
    pass

def run_task(t, args=None):
    if args is None:
        t()
    else:
        t(*args)
    run_queued_commands()
    ok()

if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("task", help="the name of the task to run")
    parser.add_argument("--confirm", help="ask for confirmation for any potentially dangerous actions", action='store_true')
    parser.add_argument("--value", help="an argument to pass to the task")
    args = parser.parse_args()
    if args.confirm:
        SHOULD_CONFIRM = True
    if args.value:
        run_task(locals()[args.task], [args.value])
    else:
        run_task(locals()[args.task])

