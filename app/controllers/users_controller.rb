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
  before_action :require_admin
  before_action :safeguard_self_modification, except: [:index]

  include StandardActions

  def model_class
    User
  end

  def obj_tag
    "User"
  end

  def define_table_view_vars
    @table_columns = { sort: :id, others: [:name, :email] }
    @controller = self.class
  end

  def safeguard_self_modification
    return unless params[:id]
    @user = User.find(params[:id])
    if @user == curr_user_obj
      redirect_to users_path and return
    end
  end
end
