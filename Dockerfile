FROM google/ruby
RUN apt-get update -qq && apt-get install -y build-essential libpq-dev node

WORKDIR /app
ADD Gemfile /app/Gemfile
RUN ["/usr/bin/bundle", "install"]
ADD . /app
