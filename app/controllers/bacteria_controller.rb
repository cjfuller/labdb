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

  @@headings = {strain_number: "#{OBJ_TAG} Number", date_entered: "Date entered", entered_by: "Entered by", notebook: "Notebook", comments: "Description", plasmid_number: "#{Naming.name_for(Plasmid)} Number", species_bkg: "Species and background", genotype: "Genotype", sequence: "Sequence", strainalias: "Alias"}


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

    @table_columns = [:strain_number, :date_entered, :entered_by, :strainalias, :plasmid_number]
    @controller = BacteriaController
    @table_objects = @bacteria

  end

  generate_standard_controller_actions(self, Bacterium, "Bacterial strain")

end
