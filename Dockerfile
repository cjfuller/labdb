FROM ruby:2.3
RUN apt-get update -qq && apt-get install -y lsb-release apt-transport-https postgresql-client libv8-dev
RUN curl -sL https://deb.nodesource.com/setup_5.x | bash -
RUN apt-get install -y nodejs
RUN echo "deb http://ftp.debian.org/debian jessie-backports main" > /etc/apt/sources.list.d/java.list
RUN apt-get update -qq && apt-get install -y build-essential libpq-dev nodejs supervisor openjdk-8-jdk
RUN gem install bundler

WORKDIR /app
ADD . /app
RUN ["bundle", "install", "--without", "development", "test", "--deployment"]
RUN ["/usr/bin/python", "manage.py", "secret"]
RUN ["/usr/bin/python", "manage.py", "hostname", "--value", "straight.labdb.io"]
CMD ["/usr/bin/supervisord", "-c", "/app/config/supervisord.conf"]
