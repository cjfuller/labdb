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

require 'exporters'

class Antibody < ActiveRecord::Base

	include Exportable

	Fields = [:ab_number, :alias, :box, :comments, :entered_by, :fluorophore, :good_for_if, :good_for_western, :host, :label, :vendor, :date_entered]

	attr_accessible *Fields

  def get_linked(propertyname)
  	nil
  end

	def exportable_fields
		Fields
	end

	def number_field
		self.send(number_field_name)
	end

	def number_field_name
		:ab_number
	end

end
