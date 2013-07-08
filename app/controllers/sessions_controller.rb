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

require 'number_assignment'

class SessionsController < ApplicationController

  skip_before_filter :require_authorization, :only => [:create, :failure, :destroy]

  def new
  end

  def create
    reset_session
    auth_hash = request.env['omniauth.auth']
    provider = auth_hash['provider']
    id = auth_hash['uid']
    user = User.find_by_uid(id)

    unless user then
      user = User.create_with_omniauth(auth_hash)
      user.save
    end
    session[:user_id] = user.uid

    redirect_to '/', notice: "You have successfully logged in."
  end

  def failure
    redirect_to '/', notice: "Authorization failed."
  end

  def destroy
    session[:user_id] = nil
    redirect_to '/', notice: "You have successfully logged out."
  end
end
