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
    redirect_to "/", notice: "Access denied."
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
    return request.query_parameters["reauth"] != "1"
    curr_uid = session[:user_id]
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

end
