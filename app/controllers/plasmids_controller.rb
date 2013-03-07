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

    puts antibiotics_string
    puts search_params

    conditions = {}

    if antibiotics_string != "" then
      conditions[:antibiotic] = antibiotics_string
    end

    conditions

  end
  
  # GET /plasmids
  # GET /plasmids.json
  def index

    define_ui_variables(status_text: "Plasmids")

    if params.has_key?(:plasmid) then
      @plasmids = process_search_query(params[:plasmid], Plasmid)
    else
      @plasmids = Plasmid.all
    end

    #NB: not doing a Plasmid.all order: "plasmidnumber" because this sorts them as strings
    @plasmids.sort! { |e0, e1| e0.plasmidnumber.to_i <=> e1.plasmidnumber.to_i }

    define_table_view_vars

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @plasmids }
    end
  end

  # GET /plasmids/1
  # GET /plasmids/1.json
  def show

    @plasmid = Plasmid.find(params[:id])

    puts @plasmid.antibiotic

    define_ui_variables(status_text: "#{obj_tag} #{@plasmid.plasmidnumber}", context_specific_buttons: "shared/top_editing_buttons", obj: @plasmid, readonly: true, show_map: true)

    @plasmid.parse_antibiotics
    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @plasmid }
    end
  end

  # GET /plasmids/new
  # GET /plasmids/new.json
  def new

    @plasmid = Plasmid.new

    define_ui_variables(status_text: "New Plasmid", readonly: false, submit_text: "Create plasmid", show_map: true)

    generate_date(@plasmid)
    generate_name(@plasmid)
    
    @plasmid.plasmidnumber = generate_object_number(Plasmid, :plasmidnumber)

    @plasmid.parse_antibiotics
    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @plasmid }
    end
  end

  # GET /plasmids/1/edit
  def edit
    @plasmid = Plasmid.find(params[:id])

    define_ui_variables(status_text: "Editing #{obj_tag} #{@plasmid.plasmidnumber}", context_specific_buttons: "shared/top_editing_buttons", obj: @plasmid, readonly: false, submit_text: "Update plasmid", show_map: true)

    @plasmid.parse_antibiotics
  end


  # POST /plasmids
  # POST /plasmids.json
  def create
    @plasmid = Plasmid.create(fix_antibiotic_params(params[:plasmid]))
    @plasmid.calculate_size
    @plasmid.antibiotic= generate_antibiotics_string(params[:plasmid])
    respond_to do |format|
      if @plasmid.save
        format.html { redirect_to @plasmid, notice: 'Plasmid was successfully created.' }
        format.json { render json: @plasmid, status: :created, location: @plasmid }
      else
        format.html { render action: "new" }
        format.json { render json: @plasmid.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /plasmids/1
  # PUT /plasmids/1.json
  def update
    @plasmid = Plasmid.find(params[:id])
    @plasmid.calculate_size
    @plasmid.antibiotic= generate_antibiotics_string(params[:plasmid])
    newparams = fix_antibiotic_params(params[:plasmid])
    respond_to do |format|
      if @plasmid.update_attributes(newparams)
        format.html { redirect_to @plasmid, notice: 'Plasmid was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @plasmid.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /plasmids/1
  # DELETE /plasmids/1.json
  def destroy
    @plasmid = Plasmid.find(params[:id])
    @plasmid.destroy

    respond_to do |format|
      format.html { redirect_to plasmids_url }
      format.json { head :no_content }
    end
  end
  
  def search
    @plasmid = Plasmid.new

    define_ui_variables(status_text: "Searching plasmids", obj: @plasmid, readonly: false, submit_text: "Search", show_map: false)

    respond_to do |format|
      format.html
      format.json { render json: @plasmid }
    end
  end

  def export

    @plasmid = Plasmid.find(params[:id])

    do_export(@plasmid)
    
  end
  
  def upload
  end
    
end
