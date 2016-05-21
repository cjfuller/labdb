FROM google/ruby
RUN apt-get update -qq && apt-get install -y lsb-release apt-transport-https postgresql-client libv8-dev
RUN curl -sL https://deb.nodesource.com/setup_5.x | bash -
RUN apt-get install -y nodejs
RUN apt-get update -qq && apt-get install -y build-essential libpq-dev nodejs

WORKDIR /app
ADD . /app
RUN ["/usr/bin/bundle", "install", "--without", "development", "test", "--deployment"]
RUN ["/usr/bin/npm", "install"]
RUN ["/usr/bin/npm", "run-script", "coffee-compile"]
RUN ["/usr/bin/npm", "run-script", "compile"]
RUN ["/usr/bin/python", "manage.py", "secret"]
RUN ["/usr/bin/python", "manage.py", "hostname", "--value", "weber.labdb.io"]
CMD RAILS_ENV=production bundle exec puma --config config/puma.rb