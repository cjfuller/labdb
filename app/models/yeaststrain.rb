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

class Yeaststrain < ActiveRecord::Base

	Fields = :antibiotic, :comments, :date_entered, :entered_by, :genotype, :location, :plasmidnumber, :sequence, :species, :strain_bkg, :strain_number, :strainalias, :notebook

	attr_accessible *Fields

	include Exportable
	include LinkableModel

	def get_linked(property_name)
		return nil unless property_name == :plasmidnumber
		return nil if self.plasmidnumber.nil?
		numbers = self.plasmidnumber.split(",").map! { |e| e.strip }
		get_linked_plasmids(numbers)
	end

		
	def exportable_fields
		Fields
	end

	def number_field
		self.strain_number
	end

	def info_field
		self.strainalias
	end

end