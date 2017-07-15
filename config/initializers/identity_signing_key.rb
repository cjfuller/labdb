key = if Rails.env.production?
          ENV['SIGNING_KEY']
        else
          'development-key'
        end

Labdb::Application.config.signing_key = key
