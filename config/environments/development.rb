require "logging"

Labdb::Application.configure do
  # Settings specified here will take precedence over those in config/application.rb

  config.eager_load = false

  # In the development environment your application's code is reloaded on
  # every request. This slows down response time but is perfect for development
  # since you don't have to restart the web server when you make code changes.
  config.cache_classes = false
  config.reload_classes_only_on_change = false

  # Show full error reports and disable caching
  config.consider_all_requests_local = true
  config.action_controller.perform_caching = false

  # Print deprecation notices to the Rails logger
  config.active_support.deprecation = :log

  # Only use best-standards-support built into browsers
  config.action_dispatch.best_standards_support = :builtin

  config.serve_static_files = true

  config.force_ssl = false

  config.log_level = :debug
  config.logger = Logging.logger

  config.js_version = ""
end
