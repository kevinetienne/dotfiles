"""
Usage:
    fab <method>
Example:
    fab deploy
"""

from fabric.api import run, cd, sudo, env, local

# uid not found
# see https://github.com/fabric/fabric/issues/400
# import pwd; pwd.getpwuid = lambda x: ["Monkey",]

# our settings
env.hosts = ['user@host']
env.directory = '/path/to/website'
env.activate = ". /usr/local/pythonenv/project/bin/activate"
env.user = "user owner"
# log
logdir = "/var/log/apache2/"
logfile = "access.project.log"
env.log = "%s%s" % (logdir, logfile)

def virtualenv():
    with cd(env.directory):
        sudo(env.activate, user=env.user)

def deploy():
    "pulls and reboot gracefully"
    with cd(env.directory):
        run("hg pull -u")
    reboot_gracefully()

def reboot_gracefully():
    "Reboot Apache2 server."
    sudo("apache2ctl graceful")

def migrate(app):
    "Migrate an app on the server"
    with cd(env.directory):
        sudo("python manage.py migrate %s" % app, user=env.user)

def revert_all():
    "mercurial revert all with graceful"
    with cd(env.directory):
        run("hg revert --all")
    reboot_gracefully()

def get_log():
    "tail log reading"
    sudo("tail -f %s| perl -pe \"s/^(?<ip>(\d+\.){3}\d+)(?<subgroup>.+)(?<method>POST|GET|PUT|DELETE|OPTIONS|TRACE|CONNECT)/\e[1;21;41m$+{ip}\e[0m$+{subgroup}\e[1;21;41m$+{method}\e[0m /g\"" % env.log)

def simple_smtp(host="localhost", port=1025):
    local("python -m smtpd -n -c DebuggingServer %s:%s" % (host, port))
