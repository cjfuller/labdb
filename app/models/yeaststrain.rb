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
require 'numbered'
require 'described'

class Yeaststrain < ActiveRecord::Base

	Fields = :antibiotic, :comments, :date_entered, :entered_by, :genotype, :location, :plasmidnumber, :sequence, :species, :strain_bkg, :strain_number, :strainalias, :notebook

	attr_accessible *Fields

	include Exportable
	include LinkableModel
	include Numbered
	include Described

	def linked_property
		:plasmidnumber
	end

	def get_linked(property_name)
		numbers = get_linked_number_fields(property_name)
		get_linked_plasmids(numbers) unless numbers.nil?
	end

	def exportable_fields
		Fields
	end

	def self.number_field_name
		:strain_number
	end

	def self.info_field_name
		:strainalias
	end

end
