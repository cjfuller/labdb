FROM google/ruby
RUN apt-get update -qq && apt-get install -y lsb-release apt-transport-https postgresql-client
RUN curl -sL https://deb.nodesource.com/setup_5.x | bash -
RUN apt-get install -y nodejs
RUN apt-get update -qq && apt-get install -y build-essential libpq-dev nodejs

WORKDIR /app
ADD Gemfile.lock /app/Gemfile.lock
RUN ["/usr/bin/bundle", "install"]
ADD . /app
RUN ["/usr/bin/npm", "install"]
RUN ["/usr/bin/npm", "run-script", "coffee-compile"]
RUN ["/usr/bin/npm", "run-script", "compile"]