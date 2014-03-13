[![Build Status](https://travis-ci.org/cjfuller/labdb.png)](https://travis-ci.org/cjfuller/labdb)

#Introduction

Labdb is a laboratory database system for simply and cleanly tracking objects like plasmids, bacterial strains, and the like.  The system is written using ruby on rails; users interact with it through a browser.

#Installation / Configuration

Prerequisites:
 - You will probably want to make a dedicated user for running the database.  This user should not have admin privileges.  Unless you have a compelling reason not to, name it "labdb".  The database will run as any user, but the auto-update script assumes this user.
 - postgresql: installation will depend on your platform; the user running the application must be able to create and edit databases.  If you want to run the tests, you will need a database user called 'postgres' as well who can create and edit (this may be done by default).  On Ubuntu, install the packages `postgresql` and `libpq-dev`.  To create a user with the name 'labdb' (you should make this the same name as the account that will run the server): `sudo -u postgres createuser -d -R -S labdb`
 - a ruby interpreter supporting ruby >= 1.9 (e.g. [http://ruby-lang.org](http://ruby-lang.org)).  We do most of our usage and testing on ruby 2.1.  To install this via [rvm](http://rvm.io), run `rvm install 2.1`.  (And then to make it the default ruby `rvm use --default 2.1`.)
 - a javascript runtime: [nodejs](http://nodejs.org) is a popular choice.  On Ubuntu, install the `nodejs` package. 
 - a python interpreter (for server management scripts), version >=2.7.  (Python 3 is ok as well.)

First, clone the repository.  You will need to add some configuration options before running the program.

 - Acquire or generate an ssl certificate and enter its path and the path of its corresponding private key in the indicated places in script/rails.  If you are generating your own, skip this step for now, and you can use a built-in script to do this later in the install process.

- Edit the text file, `config/db_names.yml` and replace the names (the part after the colon on each line) with whatever you want to call them.  The defaults are what we use in our lab, e.g. "ASP" for Aaron Straight Plasmid; plasmids would then be called ASP 1, ASP 2, etc.

 - You may also wish to check or replace the config/cacert.pem file; this is a bundle of certificate authority certificates that will be used to verify the identity of the google website used for authentication.  This was downloaded from the maintainers of the curl utility; use at your own risk.

All commands below should be run from the labdb root directory.
 
Next, install dependencies using bundler:

`bundle install`

Geneate and install an application secret token using:

`python manage.py secret`

Set up your hostname (what the server will be called; this might be "localhost" if you don't have a specific hostname configured.  It should be the address you'll access the server at minus the "https://" and the port.)

If you need to generate an ssl certificate, run:

`rake gen_cert`

This will also ensure that the certificate is in the right place for the server to find it.

If you're using this not just for development or personal use, prepare the server to run in production mode:
 - edit `config/puma.rb` and uncomment the line that says `environment = :production`.
 - run `rake assets:precompile`

Generate the database:

`rake db:setup`

Create an initial administration user:

`rake user:create[email]` where you should replace `email` with the e-mail address of the user.  (This needs to be a valid address at which you can receive mail.)  Note that the square brackets in the command are required.

Start the application:

`bundle exec puma --config config/puma.rb`

The server runs by default on port 3000 and is accessible only through https, so to visit it from the local machine, point your browser at https://localhost:3000.

In production, we manage the server using [supervisord](http://supervisord.org/).  If you set up supervisord, `manage.py` can automatically update labdb and restart the server using `python manage.py update`.

# License

Labdb is distributed under the GNU Affero General Public License, version 3.  See the file LICENSE for the full license text.




