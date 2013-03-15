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

class Plasmid < ActiveRecord::Base

  include Exportable
  include LinkableModel

  Fields = :antibiotic, :concentration, :date_entered, :description, :enteredby, :notebook, :plasmidalias, :plasmidmap, :plasmidnumber, :plasmidsize, :sequence, :strainnumbers, :vector, :verified

  attr_accessible *Fields

  has_attached_file :plasmidmap, :styles => { :thumb => ["256x256", "png"]}
  validates_attachment :plasmidmap, :content_type => {:content_type=>/image/}

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
    self.plasmidsize= self.sequence.chomp.length
  end
  
  def get_linked(property_name)
    return nil unless property_name == :strainnumbers
    return nil if self.strainnumbers.nil?
    numbers = self.strainnumbers.split(",").map! { |e| e.strip }
    get_linked_bacterial_strains(numbers)
  end


  def exportable_fields
    Fields
  end

  def number_field
    self.plasmidnumber
  end

  def info_field
    self.plasmidalias
  end

end
