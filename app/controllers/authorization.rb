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
require 'active_support/security_utils'
require 'openssl'
require 'time'

module Authorization

  ALLOWED_LABEL = "allowedusers"
  READONLY_LABEL = "readonlyusers"
  NAME_TAG = "name"
  EMAIL_TAG = "email"

  def self.included(base)
    base.class_exec do
      before_filter :require_authorization
      helper_method :auth?
    end
  end

  def denied
    render status: :forbidden, text: "Forbidden."
  end

  def require_admin
    denied unless auth? :auth_admin
    auth? :auth_admin
  end

  def require_authorization
    unless auth? :auth_write or (auth? :auth_read and request.get?) then
      denied
    end
  end

  def auth?(auth_type_key)
    #return request.query_parameters["reauth"] != "1"
    return false if curr_uid.nil?

    curr_user = User.find_by_email(curr_uid)
    return false if curr_user.nil?

    curr_user.respond_to?(auth_type_key) and curr_user.send(auth_type_key)
  end

  def update_name_from_auth_file(curr_user, u)
    if (curr_user.email == curr_user.name) then
      curr_user.name = u[NAME_TAG]
      curr_user.save
    end
  end

  def curr_uid
    if request.headers["HTTP_X_LABDB_USERID"] then
      unverified_uid = request.headers["HTTP_X_LABDB_USERID"]
      signature_timestamp = request.headers["HTTP_X_LABDB_SIGNATURE_TIMESTAMP"]
      signature_time = Time.iso8601(signature_timestamp)
      curr_time = Time.new.utc
      signature = request.headers["HTTP_X_LABDB_SIGNATURE"].downcase
      computed_signature = OpenSSL::HMAC.hexdigest(
        "SHA256", Labdb::Application.config.signing_key, unverified_uid + signature_timestamp)
      if ActiveSupport::SecurityUtils.secure_compare(signature, computed_signature) && curr_time - signature_time < 5 then
        curr = unverified_uid
      else
        curr = nil
      end
    else
      curr = session[:user_id]
    end
    curr
  end

  def current_user
    return curr_uid && User.find_by_email(curr_uid)
  end

  def auth_scope
    curr_user = current_user
    ((curr_user.send(:auth_admin) && 'admin') ||
     (curr_user.send(:auth_write) && 'write') ||
     (curr_user.send(:auth_read) && 'read') ||
     nil)
  end
end
