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

class UsersController < ApplicationController

  before_filter :require_admin
  before_filter :safeguard_self_modification, except: [:index]

  def safeguard_self_modification
    return unless params[:id]
    @user = User.find(params[:id])
    if @user == curr_user_obj then
      redirect_to users_path and return
    end
  end

  def index
    @objs = User.order("name ASC")
    @users = @objs
    respond_to do |format|
      format.html
      format.json { render json: @objs }
    end
  end

  def toggle(auth_type)
    redirect_to users_path
    if request.put? then
      user = User.find(params[:id])
      return unless user
      user.toggle_auth_field!(auth_type)
    end
  end

  def new
    @user = User.new
  end

  def create
    @user = User.new(params[:user])
    @user.save
    redirect_to users_path
  end

  def edit
    @user = User.find(params[:id])
  end

  def update
    @user = User.find(params[:id])
    @user.update_attributes(params[:user])
    redirect_to users_path
  end

  def destroy
    @user = User.find(params[:id])
    @user.destroy
    respond_to do |format|
      format.html { redirect_to users_path }
      format.json { head :no_content }
    end
  end

  def toggle_auth_read
    toggle(:auth_read)
  end

  def toggle_auth_write
    toggle(:auth_write)
  end

  def toggle_auth_admin
    toggle(:auth_admin)
  end

end
