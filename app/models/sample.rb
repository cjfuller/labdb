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
require 'headings'

class Sample < ActiveRecord::Base

	Fields = [:date_entered, :depleted, :description, :entered_by, :linked_items, :notebook, :sample_alias, :sample_number, :sample_type, :storage_type]

	attr_accessible *Fields

	include Exportable
	include LinkableModel
	include Numbered
	include Described
	include Headings

	@headings = {date_entered: "Date entered", depleted: "Sample depleted?", description: "Description", entered_by: "Entered by", linked_items: "Linked to", notebook: "Notebook", sample_alias: "Alias", sample_number: "#{obj_tag} number", sample_type: "Sample type", storage_type: "Storage location", plasmid_numbers: "Linked plasmids", strain_numbers: "Linked strains", linked_sample_numbers: "Linked samples"}

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

	def description_field_name
    :description
  end
 
  def core_alt_field
    self.depleted ? "Depleted" : nil
  end

  def sample_links
  	links = []
  	LINK_METHODS.each_key do |k|
    	k_links = get_linked(k)
    	k_links.each do |lnk_num, lnk|
    		next unless lnk
    		links << {link_text: Naming.name_for(lnk.class) + " " + lnk_num, link_obj: lnk, link_desc: lnk.info_field }
    	end
    end
    links
  end

  def groups
    {sidebar: [:entered_by, :date_entered, :notebook],
    	"Sample storage" => [:sample_type, :storage_type]}
  end

  def as_json
    return JSON.generate({
      type: "sample",
      resourceBase: "/samples",
      name: named_number_string,
      shortDescHTML: info_field.labdb_auto_link.html_safe,
      coreInfoSections: [
        {name: "Sample storage",
         fields: [
           {name: "Sample type", value: sample_type},
           {name: "Storage location", value: storage_type}
         ]},
        {name: "Description",
         preformatted: true,
         inlineValue: Labdb::Application::MARKDOWN.render(description).labdb_auto_link.html_safe},
        {name: "Linked items",
         preformatted: true,
         inlineValue: sample_links.map{|lnk| lnk[:link_text] + ": " + lnk[:link_desc]}.join("<br />").labdb_auto_link.html_safe}
      ],
      supplementalFields: [
        {name: "Entered by", value: entered_by},
        {name: "Date", value: date_entered},
        {name: "Notebook", value: notebook},
      ],
    })

  end

end
