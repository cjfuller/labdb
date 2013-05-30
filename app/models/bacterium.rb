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

class Bacterium < ActiveRecord::Base

  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include DNASequence

  Fields = [:comments, :date_entered, :entered_by, :genotype, :notebook, :plasmid_number, :species_bkg, :strain_number, :sequence, :strainalias]

  attr_accessible *Fields

  @headings = {strain_number: "#{obj_tag} Number", date_entered: "Date entered", entered_by: "Entered by", notebook: "Notebook", comments: "Description", plasmid_number: "#{Naming.name_for(Plasmid)} Number", species_bkg: "Species and background", genotype: "Genotype", sequence: "Sequence", strainalias: "Alias"}

  def linked_properties
    [:plasmid_number]
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

  def description_field_name
    :comments
  end

  def core_alt_field
    numbers = get_linked_number_fields(:plasmid_number)
    numbers.map { |n| "#{Naming.name_for(Plasmid) + " " + n.to_s}" }
  end

  def core_alt_link
    links = get_linked(:plasmid_number)
    get_linked_number_fields(:plasmid_number).map { |n| links[n] }
  end
  
  def groups
    {sidebar: [:entered_by, :date_entered, :notebook],
      "Strain information" => [:species_bkg, :genotype]}
  end

end
