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
require 'headings'
require 'resource_helpers'

class User < ActiveRecord::Base
  include Headings
  include ResourceHelpers

  Fields = :name, :email, :auth_read, :auth_write, :auth_admin, :notes
  attr_accessible(*Fields)

  has_one :search

  @headings = {
    email: "E-mail",
    auth_read: "Read access",
    auth_write: "Write access",
    auth_admin: "Admin access",
    name: "Name",
    notes: "Notes",
  }

  def exportable_fields
    [:name, :email, :auth_read, :auth_write, :auth_admin]
  end

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

  def named_number_string
    name
  end

  def self.number_field_name
    :id
  end

  def number_field_name
    self.class.number_field_name
  end

  def timestamp_field_name
    :created_at
  end

  def info_field_name
    :email
  end

  def core_info
    [
      {name: "Permissions",
       fields: [
         field(:auth_read, type: :boolean),
         field(:auth_write, type: :boolean),
         field(:auth_admin, type: :boolean),
       ]},
    ]
  end

  def sequence_info
    nil
  end

  def supplemental_info
    [
      field(:name),
      field(:email),
      field(:notes)
    ]
  end

end
