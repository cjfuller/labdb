FROM ruby:2.3
RUN apt-get update -qq && apt-get install -y lsb-release apt-transport-https postgresql-client libv8-dev
RUN curl -sL https://deb.nodesource.com/setup_5.x | bash -
RUN apt-get install -y nodejs
RUN apt-get update -qq && apt-get install -y build-essential libpq-dev nodejs supervisor
RUN gem install bundler

WORKDIR /app
ADD . /app
RUN ["bundle", "install", "--without", "development", "test", "--deployment"]
RUN ["/usr/bin/python", "manage.py", "secret"]
RUN ["/usr/bin/python", "manage.py", "hostname", "--value", "weber.labdb.io"]
CMD ["/usr/bin/supervisord", "-c", "/app/config/supervisord.conf"]
