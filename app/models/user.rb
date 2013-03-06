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

class User < ActiveRecord::Base

  attr_protected :name, :provider, :uid, :email
  
  def self.create_with_omniauth(auth)
    user = User.new
    user.provider = auth['provider']
    user.uid = auth['uid']
    user.email = auth['info']['email']
    user.name = auth['info']['name']
    user.save
    user
  end



end
