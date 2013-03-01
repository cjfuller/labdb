require 'omniauth-openid'

require 'openid/fetchers'

Rails.application.config.middleware.use Rack::Session::Cookie

OpenID.fetcher.ca_file = Rails.root.join("config/cacert.pem").to_s

Rails.application.config.middleware.use OmniAuth::Builder do
  provider :openid, name: 'google', identifier: 'https://www.google.com/accounts/o8/id'
end
