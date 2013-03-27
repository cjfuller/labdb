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

require 'object_naming'

class PlasmidsController < ApplicationController

  before_filter :antibiotic_params_from_form_for_create, only: [:create]
  before_filter :antibiotic_params_from_form, only: [:update]

  OBJ_TAG = Naming.name_for(Plasmid)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {:plasmidnumber => "#{OBJ_TAG} Number", :date_entered => "Date entered",
    :enteredby => "Entered by", :notebook => "Notebook", :verified => "Sequence verified?",
    :plasmidalias => "Alias", :antibiotic => "Antibiotic resistances", :plasmidsize => "Size",
    :concentration => "Concentration (ug/mL)", :strainnumbers => "#{Naming.name_for(Bacterium)} numbers",
    :description => "Description", :sequence => "Sequence", :vector => "Vector",
    :mapreference => "Map"}
  
  @@plasmid_number_mutex = Mutex.new
  
  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/plasmids/search"
  end

  def define_ui_variables(params)
    params[:search_path] = search_path
    params[:model_class] = Plasmid
    super(params)
  end

  def define_table_view_vars
    
    @table_columns = [:plasmidnumber, :date_entered, :enteredby, :plasmidalias, :plasmidsize, :strainnumbers]
    @controller = PlasmidsController
    @table_objects = @plasmids

  end
  
  def preprocess_model_object(model_obj)
    model_obj.parse_antibiotics
    model_obj.calculate_size
  end

  def antibiotic_params_from_form_for_create

    return nil if params[:plasmid][:antibiotic] and params[:plasmid][:antibiotic].size > 0

    antibiotic_params_from_form

  end

  def antibiotic_params_from_form

    antibiotic_string = generate_antibiotics_string(params[:plasmid])

    params[:plasmid] = fix_antibiotic_params(params[:plasmid])

    params[:plasmid][:antibiotic] = antibiotic_string

  end


  def fix_antibiotic_params(param_hash)
    
    abs = Plasmid.get_antibiotics

    newparams = Hash.new
    
    param_hash.each_key do |k|
      unless abs.has_value?(k) then
        newparams[k] = param_hash[k]
      end
    end
    
    newparams
    
  end
  
  def generate_antibiotics_string(params_hash)
    
    antibiotic=""
    
    Plasmid.get_antibiotics.each_value do |v|
      if params_hash[v] == "1" then
      
        if antibiotic.length > 0 then
          antibiotic= antibiotic + ","
        end
        
        antibiotic = antibiotic + v
        
      end
        
    end
    
    antibiotic
    
  end

  def preprocess_search_query(search_params)

    antibiotics_string = generate_antibiotics_string(search_params)
    mod_search_params = fix_antibiotic_params(search_params)

    search_params.delete_if { |e| not (mod_search_params.include?(e)) }

    conditions = {}

    if antibiotics_string != "" then
      conditions[:antibiotic] = antibiotics_string
    end

    conditions

  end
  
  generate_standard_controller_actions(self, Plasmid, "Plasmid")
  
  def upload
  end
    
end
