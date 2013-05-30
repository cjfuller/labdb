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

end
