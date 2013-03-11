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

class AntibodiesController < ApplicationController

  OBJ_TAG = Naming.name_for(Antibody)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {:ab_number => "#{OBJ_TAG} Number", :date_entered => "Date entered", :label => "Label",
                :entered_by => "Entered by", :alias => "Alias", :comments => "Description", :host => "Host",
                :vendor => "Vendor", :good_for_if => "Good for IF?", :good_for_western => "Good for westerns?",
                :fluorophores => "Fluorophores", :box => "Box"}


  GOOD_FOR_PARAMS = ["good_for_if", "good_for_western"]


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/antibodies/search"
  end

  def fix_goodfor_params(param_hash)

    newparams = Hash.new

    param_hash.each_key do |k|
      unless GOOD_FOR_PARAMS.include?(k) then
        newparams[k] = param_hash[k]
      end
    end

    newparams

  end

  def generate_goodfor_conditions(params_hash)

    good_for = {}

    GOOD_FOR_PARAMS.each do |v|
      if params_hash[v] == "1" then

        good_for[v] = "true"

      end

    end

    good_for

  end

  def preprocess_search_query(search_params)

    conditions = generate_goodfor_conditions(search_params)

    mod_search_params = fix_goodfor_params(search_params)

    search_params.delete_if { |e| not (mod_search_params.include?(e)) }

    conditions

  end

  def define_ui_variables(params)

    params[:model_class]= Antibody
    params[:search_path]= search_path

    super(params)

  end


  def define_table_view_vars

    @table_columns = [:ab_number, :date_entered, :entered_by, :alias, :host]
    @controller = AntibodiesController
    @table_objects = @antibodies

  end


  # GET /antibodies
  # GET /antibodies.json
  def index

    define_ui_variables(status_text: "Antibodies")

    if params.has_key?(:antibody) then
      @antibodies = process_search_query(params[:antibody], Antibody)
    else
      @antibodies = Antibody.all
    end

    @antibodies.sort! { |e0, e1| e0.ab_number.to_i <=> e1.ab_number.to_i }

    define_table_view_vars

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @antibodies }
    end
  end

  # GET /antibodies/1
  # GET /antibodies/1.json
  def show
    @antibody = Antibody.find(params[:id])

    define_ui_variables(status_text: "#{obj_tag} #{@antibody.ab_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @antibody, readonly: true)

    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @antibody }
    end
  end

  # GET /antibodies/new
  # GET /antibodies/new.json
  def new

    @antibody = Antibody.new

    define_ui_variables(status_text: "New antibody", readonly: false, submit_text: "Create antibody")

    generate_date(@antibody)
    generate_name(@antibody)
    @antibody.ab_number = generate_object_number(Antibody, :ab_number)


    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @antibody }
    end
  end

  # GET /antibodies/1/edit
  def edit

    @antibody = Antibody.find(params[:id])

    define_ui_variables(status_text: "Editing #{obj_tag} #{@antibody.ab_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @antibody, readonly: false, submit_text: "Update antibody")

  end

  # POST /antibodies
  # POST /antibodies.json
  def create
    @antibody = Antibody.new(params[:antibody])

    respond_to do |format|
      if @antibody.save
        format.html { redirect_to @antibody, notice: 'Antibody was successfully created.' }
        format.json { render json: @antibody, status: :created, location: @antibody }
      else
        format.html { render action: "new" }
        format.json { render json: @antibody.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /antibodies/1
  # PUT /antibodies/1.json
  def update
    @antibody = Antibody.find(params[:id])

    respond_to do |format|
      if @antibody.update_attributes(params[:antibody])
        format.html { redirect_to @antibody, notice: 'Antibody was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @antibody.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /antibodies/1
  # DELETE /antibodies/1.json
  def destroy
    @antibody = Antibody.find(params[:id])
    @antibody.destroy

    respond_to do |format|
      format.html { redirect_to antibodies_url }
      format.json { head :no_content }
    end
  end


  def search
    @antibody = Antibody.new

    define_ui_variables(status_text: "Searching antibodies", obj: @antibody, readonly: false, submit_text: "Search")

    respond_to do |format|
      format.html
      format.json { render json: @oligo }
    end
  end

  def export

    @antibody = Antibody.find(params[:id])

    do_export(@antibody)
    
  end

end
