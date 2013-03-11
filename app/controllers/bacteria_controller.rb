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

class BacteriaController < ApplicationController

  OBJ_TAG = Naming.name_for(Bacterium)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {strain_number: "#{OBJ_TAG} Number", date_entered: "Date entered", entered_by: "Entered by", notebook: "Notebook", comments: "Description", plasmid_number: "#{Naming.name_for(Plasmid)} Number", species_bkg: "Species and background", genotype: "Genotype"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/bacteria/search"
  end

  def define_ui_variables(params)

    params[:model_class]= Bacterium
    params[:search_path]= search_path

    super(params)

  end

  def define_table_view_vars

    @table_columns = [:strain_number, :date_entered, :entered_by, :species_bkg, :plasmid_number]
    @controller = BacteriaController
    @table_objects = @bacteria

  end


  # GET /bacteria
  # GET /bacteria.json
  def index

    define_ui_variables(status_text: "Bacterial strains", context_specific_buttons: "shared/top_pagination_buttons")

    page_size = 250
    params[:page] = 1 unless params[:page]

    if params.has_key?(:bacterium) then
      @bacteria = process_search_query(params[:bacterium], Bacterium)
      page_size = @bacteria.size
    else
      @bacteria = Bacterium.all
    end

    @bacteria.sort! { |e0, e1| e0.strain_number.to_i <=> e1.strain_number.to_i }

    @bacteria = Kaminari.paginate_array(@bacteria).page(params[:page]).per(page_size)

    define_table_view_vars

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @bacteria }
    end
  end

  # GET /bacteria/1
  # GET /bacteria/1.json
  def show

    @bacterium = Bacterium.find(params[:id])

    define_ui_variables(status_text: "#{obj_tag} #{@bacterium.strain_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @bacterium, readonly: true)


    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @bacterium }
    end
  end

  # GET /bacteria/new
  # GET /bacteria/new.json
  def new

    @bacterium = Bacterium.new

    define_ui_variables(status_text: "New bacterial strain", readonly: false, submit_text: "Create strain")

    generate_date(@bacterium)
    generate_name(@bacterium)
    @bacterium.strain_number = generate_object_number(Bacterium, :strain_number)


    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @bacterium }
    end
  end

  # GET /bacteria/1/edit
  def edit

    @bacterium = Bacterium.find(params[:id])

     define_ui_variables(status_text: "Editing #{obj_tag} #{@bacterium.strain_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @bacterium, readonly: false, submit_text: "Update strain")

  end

  # POST /bacteria
  # POST /bacteria.json
  def create
    @bacterium = Bacterium.new(params[:bacterium])

    respond_to do |format|
      if @bacterium.save
        format.html { redirect_to @bacterium, notice: 'Bacterium was successfully created.' }
        format.json { render json: @bacterium, status: :created, location: @bacterium }
      else
        format.html { render action: "new" }
        format.json { render json: @bacterium.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /bacteria/1
  # PUT /bacteria/1.json
  def update
    @bacterium = Bacterium.find(params[:id])

    respond_to do |format|
      if @bacterium.update_attributes(params[:bacterium])
        format.html { redirect_to @bacterium, notice: 'Bacterium was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @bacterium.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /bacteria/1
  # DELETE /bacteria/1.json
  def destroy
    @bacterium = Bacterium.find(params[:id])
    @bacterium.destroy

    respond_to do |format|
      format.html { redirect_to bacteria_url }
      format.json { head :no_content }
    end
  end

  def search
    @bacterium = Bacterium.new

    define_ui_variables(status_text: "Searching bacterial strains", obj: @bacterium, readonly: false, submit_text: "Search")

    respond_to do |format|
      format.html
      format.json { render json: @bacterium }
    end
  end

  def export

    @bacterium = Bacterium.find(params[:id])

    do_export(@bacterium)

  end

end
