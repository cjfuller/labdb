# encoding: utf-8
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

require 'json'

require 'exporters'
require 'numbered'
require 'described'
require 'headings'
require 'dna_sequence'
require 'plasmid_mapping'

require 'facets/string/word_wrap'

class Plasmid < ActiveRecord::Base

  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include DNASequence

  Fields = :antibiotic, :concentration, :date_entered, :description, :enteredby, :notebook, :plasmidalias, :plasmidmap, :plasmidnumber, :plasmidsize, :sequence, :strainnumbers, :vector, :verified

  @headings = {:plasmidnumber => "#{obj_tag} Number", :date_entered => "Date",
    :enteredby => "Entered by", :notebook => "Notebook", :verified => "Sequence verified?",
    :plasmidalias => "Alias", :antibiotic => "Antibiotic resistances", :plasmidsize => "Size",
    :concentration => "Concentration (μg/mL)", :strainnumbers => "#{Naming.name_for(Bacterium)}",
    :description => "Description", :sequence => "Sequence", :vector => "Vector",
    :mapreference => "Map"}

  attr_accessible *Fields

  attr_accessor :antibiotics

  @@Antibiotics = {"carbenicillin" => "carb", "kanamycin"=>"kan", "chloramphenicol"=>"chlor", "gentamycin"=>"gent", "tetracycline"=>"tet", "streptomycin"=>"strep"}

  def self.get_antibiotics
    return @@Antibiotics
  end

  self.get_antibiotics.each_value do |v|
    attr_accessor v.to_sym
  end
  
  attr_accessor :search_by_regex

  def parse_antibiotics
    @@Antibiotics.each_value do |v|
      self.send((v + "=").to_sym, "0")
    end
    
    if not self.antibiotic or self.antibiotic.length == 0 then
      return
    end
    
    self.antibiotic.split(",").each do |ab|
      if self.respond_to?(ab.to_sym) then
        self.send((ab+"=").to_sym, "1")
      end
    end
  end
  
  def calculate_size
    self.plasmidsize= self.sequence.gsub(/\W/, "").length
    self.save
  end
  
  def linked_properties
    [:strainnumbers]
  end

  def get_linked(property_name)
    numbers = get_linked_number_fields(property_name)
    get_linked_bacterial_strains(numbers) unless numbers.nil?
  end

  def exportable_fields
    Fields.reject { |e| e == :plasmidmap }
  end

  def self.number_field_name
    :plasmidnumber
  end

  def self.info_field_name
    :plasmidalias
  end

  def description_field_name
    :description
  end

  def core_alt_field
    numbers = get_linked_number_fields(:strainnumbers)
    numbers.map { |n| "#{Naming.name_for(Bacterium) + " " + n.to_s}" }
  end

  def core_alt_link
    links = get_linked(:strainnumbers)
    get_linked_number_fields(:strainnumbers).map { |n| links[n] }
  end

  def groups
    {sidebar: [:enteredby, :date_entered, :notebook, :concentration],
      "Vector information" => [:vector, :antibiotic]}
  end

  def map
    @map or (@map = PlasmidMapping.map_for_plasmid(self).plasmidmap_json)
  end

  def as_json
    return JSON.generate({
      type: "plasmid",
      resourceBase: "/plasmids",
      name: named_number_string,
      shortDescHTML: info_field.labdb_auto_link.html_safe,
      coreLinksHTML: core_alt_field.map(&:labdb_auto_link),
      coreInfoSections: [
        {name: "Vector information",
         fields: [
           {name: "Vector", value: vector},
           {name: "Antibiotic resistances", value: antibiotic}
         ]},
        {name: "Description",
         preformatted: true,
         inlineValue: Labdb::Application::MARKDOWN.render(description).labdb_auto_link.html_safe}
      ],
      sequenceInfo: {
        sequence: sequence,
        verified: verified,
      },
      supplementalFields: [
        {name: "Entered by", value: enteredby},
        {name: "Date", value: date_entered},
        {name: "Notebook", value: notebook},
        {name: "Concentration", value: concentration},
      ],
    })
  end
end
