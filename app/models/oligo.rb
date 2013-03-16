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


class Oligo < ActiveRecord::Base

	include Exportable

  Fields = [:oligoalias, :date_entered, :entered_by, :notebook, :oligo_number, :organism, :purpose, :sequence, :vendor]

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
		:oligo_number
	end

	def info_field
		self.send(info_field_name)
	end

	def info_field_name
		:oligoalias
	end

end
