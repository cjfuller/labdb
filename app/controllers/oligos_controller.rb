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

class OligosController < ApplicationController

  OBJ_TAG = Naming.name_for(Oligo)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {:oligo_number => "#{OBJ_TAG} Number", :date_entered => "Date entered", :entered_by => "Entered by", :notebook => "Notebook", :oligoalias => "Alias", :purpose => "Description", :sequence => "Sequence", :organism => "Organism", :vendor => "Vendor"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def search_path
    "/oligos/search"
  end

  def define_ui_variables(params)

    params[:model_class]= Oligo
    params[:search_path]= search_path

    super(params)

  end

  def define_table_view_vars

    @table_columns = [:oligo_number, :date_entered, :entered_by, :oligoalias]
    @controller = OligosController
    @table_objects = @oligos

  end


  # GET /oligos
  # GET /oligos.json
  def index

    define_ui_variables(status_text: "Oligos", context_specific_buttons: "shared/top_pagination_buttons")

    page_size = 250
    params[:page] = 1 unless params[:page]

    if params.has_key?(:oligo) then
      @oligos = process_search_query(params[:oligo], Oligo)
    else
      @oligos = Oligo.all
    end

    @oligos.sort! { |e0, e1| e0.oligo_number.to_i <=> e1.oligo_number.to_i }

    @oligos = Kaminari.paginate_array(@oligos).page(params[:page]).per(page_size)

    define_table_view_vars

    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @oligos }
    end
  end

  # GET /oligos/1
  # GET /oligos/1.json
  def show
    @oligo = Oligo.find(params[:id])

    define_ui_variables(status_text: "#{obj_tag} #{@oligo.oligo_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @oligo, readonly: true)

    respond_to do |format|
      format.html # show.html.erb
      format.json { render json: @oligo }
    end
  end

  # GET /oligos/new
  # GET /oligos/new.json
  def new

    @oligo = Oligo.new

    define_ui_variables(status_text: "New oligo", readonly: false, submit_text: "Create oligo")

    generate_date(@oligo)
    generate_name(@oligo)
    @oligo.oligo_number = generate_object_number(Oligo, :oligo_number)


    respond_to do |format|
      format.html # new.html.erb
      format.json { render json: @oligo }
    end
  end

  # GET /oligos/1/edit
  def edit

    @oligo = Oligo.find(params[:id])

    define_ui_variables(status_text: "Editing #{obj_tag} #{@oligo.oligo_number}", context_specific_buttons: "shared/top_editing_buttons", obj: @oligo, readonly: false, submit_text: "Update oligo")


  end

  # POST /oligos
  # POST /oligos.json
  def create
    @oligo = Oligo.new(params[:oligo])

    respond_to do |format|
      if @oligo.save
        format.html { redirect_to @oligo, notice: 'Oligo was successfully created.' }
        format.json { render json: @oligo, status: :created, location: @oligo }
      else
        format.html { render action: "new" }
        format.json { render json: @oligo.errors, status: :unprocessable_entity }
      end
    end
  end

  # PUT /oligos/1
  # PUT /oligos/1.json
  def update
    @oligo = Oligo.find(params[:id])

    respond_to do |format|
      if @oligo.update_attributes(params[:oligo])
        format.html { redirect_to @oligo, notice: 'Oligo was successfully updated.' }
        format.json { head :no_content }
      else
        format.html { render action: "edit" }
        format.json { render json: @oligo.errors, status: :unprocessable_entity }
      end
    end
  end

  # DELETE /oligos/1
  # DELETE /oligos/1.json
  def destroy
    @oligo = Oligo.find(params[:id])
    @oligo.destroy

    respond_to do |format|
      format.html { redirect_to oligos_url }
      format.json { head :no_content }
    end
  end

  def search
    @oligo = Oligo.new

    define_ui_variables(status_text: "Searching oligos", obj: @oligo, readonly: false, submit_text: "Search")

    respond_to do |format|
      format.html
      format.json { render json: @oligo }
    end
  end

  def export

    @oligo = Oligo.find(params[:id])

    do_export(@oligo)

  end

end
