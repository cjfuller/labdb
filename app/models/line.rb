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
require 'headings'
require 'dna_sequence'
require 'resource_helpers'

require 'json'

class Line < ActiveRecord::Base

  Fields = [:current_stock_counts, :date_entered, :description, :entered_by, :line_alias, :line_number, :locations, :notebook, :parent_line, :plasmid_numbers, :selectable_markers, :sequence, :species, :genotype, :stock_person, :stock_date]

  InvFields = [:current_stock_counts, :locations, :stock_person, :stock_date]

	attr_accessible *Fields

	include Exportable
	include LinkableModel
	include Numbered
	include Described
	include Headings
	include DNASequence
  include ResourceHelpers

	@headings = {current_stock_counts: "Stock counts", date_entered: "Date entered", description: "Description", entered_by: "Entered by", line_alias: "Alias", line_number: "#{obj_tag} number", locations: "Locations", notebook: "Notebook", parent_line: "Parent line", plasmid_numbers: "#{Naming.name_for(Plasmid)} numbers", selectable_markers: "Selectable markers", sequence: "Associated sequence", species: "Species", genotype: "Genotype", stock_person: "Person", stock_date: "Date", stock_clone: "Clone"}



	class InventoryItem

		Fields = [:location, :count, :person, :date, :clone]

		attr_accessor *Fields

		def json
			{location: self.location, count: self.count, person: self.person, date: self.date, clone: self.clone}.to_json
		end

		def self.from_hash(hash)
			ii = InventoryItem.new
			hash.each { |k,v| ii.send(k.to_s + "=", v) }
            begin
			    ii.date = Date.parse(ii.date.to_s) if ii.date.to_s.size > 0
            rescue ArgumentError => e
                puts "Invalid date"
                ii.date = nil
            end
			ii
		end

		def self.from_json(str)
			fields = JSON.parse(str)
			from_hash(fields)
		end

		def ==(other)
			Fields.all? { |f| self.send(f) == other.send(f) }
		end

	end

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

	def description_field_name
    :description
  end

  def core_alt_field_name
    :plasmid_numbers
  end

  def owner_field_name
    :entered_by
  end

  def timestamp_field_name
    :date_entered
  end

  def core_alt_field
    numbers = get_linked_number_fields(:plasmid_numbers) || []
    numbers.map { |n| "#{Naming.name_for(Plasmid) + " " + n.to_s}" }
  end

  def core_alt_link
    links = get_linked(:plasmid_numbers)
    (get_linked_number_fields(:plasmid_numbers) || []).map { |n| links[n] }
  end

  def groups
    {sidebar: [:entered_by, :date_entered, :notebook],
      "Line information" => [:species, :genotype, :selectable_markers, :parent_line]}
  end

  def core_info
    [
      {name: "Line information",
       fields: fields([:species, :genotype, :selectable_markers, :parent_line])},
      {name: "Description",
       preformatted: true,
       single: true,
       lookup: :description,
       inlineValue: Labdb::Application::MARKDOWN.render(description || "").labdb_auto_link.html_safe}
    ]
  end

  def sequence_info
    {sequence: {lookup: :sequence},
     verified: nil}
  end

  def supplemental_info
    fields([:entered_by, :date_entered, :notebook])
  end

	def inventory

		inv = []

		return inv unless self.locations

		locs = self.locations.split(",")
		counts = self.current_stock_counts.split(",")

		self.stock_person ||= ","*(self.locations.count(","))
		self.stock_date ||= ","*(self.locations.count(","))

		people = self.stock_person.split(",")
		dates = self.stock_date.split(",")
		clones = counts.map { "" }

		counts.each.with_index do |c,i|
			if /\s*\d+\s*\(\s*clone\s*\d+\)/.match(c) then
				matchobj = /\s*(\d+)\s*\(\s*clone\s*(\d+)\s*\)/.match(c)
				counts[i] = matchobj[1]
				clones[i] = matchobj[2]
			end
		end

		locs.each_index do |i|
			inv_item = InventoryItem.new
			inv_item.location = locs[i]
			inv_item.count = counts[i].to_i
			inv_item.clone = clones[i]
			inv_item.person = people[i]
			inv_item.date = ((not dates[i].nil?) and dates[i].size > 0) ? Date.parse(dates[i]) : ""
			inv << inv_item
		end

		inv= inv.sort { |e1, e2| e1.date <=> e2.date or 0 }

	end

	def update_inventory(inv)

		inv= inv.sort { |e1, e2| e1.date <=> e2.date or 0 }

		counts = inv.map { |e| e.count }
		locs = inv.map { |e| e.location }
		clones = inv.map { |e| e.clone }
		people = inv.map { |e| e.person }
		date = inv.map do |e|
			begin
				Date.parse(e.date.to_s).to_s
			rescue
				""
			end
		end


		self.locations = locs.join(",")
		self.current_stock_counts = counts.map.with_index do |e, i|
			clone_string = ""
			if clones[i].length > 0 then
				clone_string = "(clone #{clones[i]})"
			end
			e.to_s + clone_string
		end
		self.current_stock_counts = self.current_stock_counts.join(",")

		self.stock_person = people.join(",")
		self.stock_date = date.join(",")

	end

end
