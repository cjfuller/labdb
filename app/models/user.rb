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

  attr_protected :email, :auth_read, :auth_write, :auth_admin

  attr_accessible :name, :email, :notes

  has_one :search

  def toggle_auth_field!(auth_type)
    self.send(auth_type.to_s + "=", (not self.send(auth_type)))
    self.save
  end

  def auth_admin!(should_auth)
    self.auth_admin = should_auth
    self.save
  end

  def auth_read!(should_auth)
    self.auth_read = should_auth
    self.save
  end

  def auth_write!(should_auth)
    self.auth_write = should_auth
    self.save
  end
end
