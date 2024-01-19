FROM ruby:2.7
RUN apt-get update -qq && apt-get install -y lsb-release apt-transport-https postgresql-client libv8-dev libpq-dev build-essential
RUN gem install bundler -v 2.4.22

WORKDIR /app
ADD . /app
RUN ["bundle", "install", "--without", "development", "test", "--deployment"]
ARG JS_VERSION
ENV JS_VERSION ${JS_VERSION}
ENV RACK_ENV production
ENV RAILS_ENV production
CMD ["bundle", "exec", "puma", "--config", "config/puma.rb"]
