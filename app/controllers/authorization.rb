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
  NAME_TAG = "name"
  EMAIL_TAG = "email"

	def self.included(base)
		base.class_exec do 
			before_filter :require_authorization
		end
	end

	def denied
    render :text => "Access denied."
  end

  def require_authorization

    unless authorized? then

      redirect_to "/", notice: "Access denied."

    end

  end

  def load_auth

    @user_data = Rails.configuration.user_data

    @users_loaded = true

  end

  def authorized?

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    return false if curr_user.nil?

    load_auth unless @users_loaded

    allowed_users = @user_data[ALLOWED_LABEL]

    allowed_users.each do |u|

      if curr_user.email == u[EMAIL_TAG] and curr_user.name == u[NAME_TAG] then
        return true
      end

    end

    false

  end


end

