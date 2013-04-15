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
require 'object_naming'

class Sample < ActiveRecord::Base

	Fields = [:date_entered, :depleted, :description, :entered_by, :linked_items, :notebook, :sample_alias, :sample_number, :sample_type, :storage_type]

	attr_accessible *Fields

	include Exportable
	include LinkableModel
	include Numbered
	include Described

	LINK_METHODS = {plasmid_numbers: :get_linked_plasmids, strain_numbers: :get_linked_bacterial_strains, linked_sample_numbers: :get_linked_samples}

	def linked_properties
		[:plasmid_numbers, :strain_numbers, :linked_sample_numbers]
	end

	def parse_numbers(name)
		return nil if self.linked_items.nil?
		match_exp = /#{name}\s*(\d+)/
		all_items = self.linked_items.split(",")
		matching_numbers = []
		all_items.each do |item|
			matchobj = match_exp.match(item)
			matching_numbers << matchobj[1] if matchobj
		end
		matching_numbers.join(",")
	end

	def plasmid_numbers
		name = Naming.name_for(Plasmid)
		parse_numbers(name)
	end

	def strain_numbers
		name = Naming.name_for(Bacterium)
		parse_numbers(name)
	end

	def linked_sample_numbers
		name = Naming.name_for(Sample)
		parse_numbers(name)
	end

	def get_linked(property_name)
		numbers = get_linked_number_fields(property_name)
		self.send(LINK_METHODS[property_name], numbers) unless numbers.nil?
	end
		
	def exportable_fields
		Fields
	end

	def self.number_field_name
		:sample_number
	end

	def self.info_field_name
		:sample_alias
	end

end
