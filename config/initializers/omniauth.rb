#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

require 'omniauth-openid'

require 'openid/fetchers'

HOSTNAME_FILE = "config/full_hostname.txt"

Rails.application.config.middleware.use Rack::Session::Cookie

OmniAuth.config.full_host = File.read("config/full_hostname.txt")

OpenID.fetcher.ca_file = Rails.root.join("config/cacert.pem").to_s

Rails.application.config.middleware.use OmniAuth::Builder do
  provider :openid, name: 'google', identifier: 'https://www.google.com/accounts/o8/id'
  provider :browser_id, name: 'persona', verify_url: 'https://login.persona.org/verify'
end
