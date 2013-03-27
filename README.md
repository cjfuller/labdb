[![Build Status](https://travis-ci.org/cjfuller/labdb.png)](https://travis-ci.org/cjfuller/labdb)

#Introduction

Labdb is a laboratory database system for simply and cleanly tracking objects like plasmids, bacterial strains, and the like.  The system is written using ruby on rails; users interact with it through a browser.

#Installation / Configuration

Prerequisites: postgresql (installation will depend on your platform; the user running the application must be able to create and edit databases), a ruby interpreter supporting ruby >= 1.9 (e.g. [http://ruby-lang.org](http://ruby-lang.org)).

First, clone the repository.  You will need to add some configuration options before running the program.

 - Acquire or generate an ssl certificate and enter its path and the path of its corresponding private key in the indicated places in script/rails.

 - Edit config/authusers.yml and add the names and e-mail addresses of the users who will be able to access the database.  Login is done through google, so these must be the exact names and e-mails associated with the users' google accounts.

 - You may also wish to check or replace the config/cacert.pem file; this is a bundle of certificate authority certificates that will be used to verify the identity of the google website used for authentication.  This was downloaded from the maintainers of the curl utility; use at your own risk.

 
Next, install dependencies using bundler:

`bundle install`

Geneate an application secret token using:

`bundle exec rake secret`

and edit config/initializers/secret_token.rb and put the secret in the indicated place.

Generate the database:

`bundle exec rake db:setup`

Start the application:

`bundle exec rails server`

The server runs by default on port 3000 and is accessible only through https, so to visit it from the local machine, point your browser at https://localhost:3000.

# License

Labdb is distributed under the GNU Affero General Public License, version 3.  See the file LICENSE for the full license text.

The distribution also contains an unmodified copy of the bootstrap framework, which is copyright by Twitter, Inc., and distributed under the Apache License.  See the file bootstrap-license for the full text of bootstrap's license.



