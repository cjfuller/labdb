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

class Line < ActiveRecord::Base

  Fields = [:current_stock_counts, :date_entered, :description, :entered_by, :line_alias, :line_number, :locations, :notebook, :parent_line, :plasmid_numbers, :selectable_markers, :sequence, :species, :genotype]

	attr_accessible *Fields

	include Exportable
	include LinkableModel
	include Numbered
	include Described

	def linked_properties
		[:plasmid_numbers]
	end

	def get_linked(property_name)
		numbers = get_linked_number_fields(property_name)
		get_linked_plasmids(numbers) unless numbers.nil?
	end
		
	def exportable_fields
		Fields
	end

	def self.number_field_name
		:line_number
	end

	def self.info_field_name
		:line_alias
	end

	def inventory

		inv = {}

		locs = self.locations.split(",")
		counts = self.current_stock_counts.split(",")

		locs.each_with_index do |l, i|
			inv[l] = counts[i].to_i
		end

		inv

	end

	def update_inventory(inv)

		locs = inv.keys.sort
		counts = locs.map { |l| inv[l].to_s }

		self.locations = locs.join(",")
		self.current_stock_counts = counts.join(",")

	end

end
