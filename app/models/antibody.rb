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
class Antibody < ActiveRecord::Base

	include Exportable
	include Numbered
	include Described
  include Headings

	Fields = [:ab_number, :alias, :box, :comments, :entered_by, :fluorophore, :good_for_if, :good_for_western, :host, :label, :vendor, :date_entered]

	attr_accessible *Fields

	@headings = {:ab_number => "#{obj_tag} Number", :date_entered => "Date entered", :label => "Label",
                :entered_by => "Entered by", :alias => "Alias", :comments => "Description", :host => "Host",
                :vendor => "Vendor", :good_for_if => "Good for IF?", :good_for_western => "Good for westerns?",
                :fluorophore => "Fluorophores", :box => "Box"}

  def get_linked(propertyname)
  	nil
  end

	def exportable_fields
		Fields
	end

	def self.number_field_name
		:ab_number
	end

	def self.info_field_name
		:alias
	end

	def description_field_name
    :comments
  end

  def groups
    {sidebar: [:entered_by, :date_entered, :vendor],
    	"Antibody information" => [:host, :fluorophore],
    	"Location information" => [:box, :label] }
  end

  def as_json
    return JSON.generate({
      type: "antibody",
      resourceBase: "/antibodies",
      name: named_number_string,
      shortDescHTML: info_field.labdb_auto_link.html_safe,
      coreInfoSections: [
        {name: "Antibody information",
         fields: [
           {name: "Host", value: host},
           {name: "Fluorophores", value: fluorophore}
         ]},
        {name: "Location information",
         fields: [
           {name: "Box", value: box},
           {name: "Label", value: label}
         ]},
        {name: "Uses",
         fields: [
           {name: "Good for IF", value: good_for_if, type: :boolean},
           {name: "Good for westerns", value: good_for_western, type: :boolean}
         ]},
        {name: "Description",
         preformatted: true,
         inlineValue: Labdb::Application::MARKDOWN.render(comments).labdb_auto_link.html_safe}
      ],
      supplementalFields: [
        {name: "Entered by", value: entered_by},
        {name: "Date", value: date_entered},
        {name: "Vendor", value: vendor},
      ],
    })
  end
end
