require 'omniauth-openid'
require 'openid/store/filesystem'
require 'openid/fetchers'

OpenID.fetcher.ca_file = "#{Rails.root}/config/ca-bundle.crt"

Rails.application.config.middleware.use Rack::Session::Cookie

Rails.application.config.middleware.use OmniAuth::Builder do
  provider :openid, :name => 'google', :identifier => 'https://www.google.com/accounts/o8/id'
end
