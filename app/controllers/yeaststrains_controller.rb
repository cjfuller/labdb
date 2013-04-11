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

class YeaststrainsController < ApplicationController
  
  include StandardActions

  OBJ_TAG = Naming.name_for(Yeaststrain)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {strain_number: "#{OBJ_TAG} Number", date_entered: "Date entered",
                entered_by: "Entered by", notebook: "Notebook", 
                comments: "Description", plasmidnumber: "#{Naming.name_for(Plasmid)} Number", strain_bkg: "Strain background", genotype: "Genotype", antibiotic: "Antibiotics", location: "Location in freezer", sequence: "Sequence", species: "Species", strainalias: "Alias"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def self.model_class
    Yeaststrain
  end

  def self.text
    "Yeast strain"
  end

  def search_path
    "/yeaststrains/search"
  end

  def define_ui_variables(params)

    params[:model_class]= Yeaststrain
    params[:search_path]= search_path

    super(params)

  end

  def define_table_view_vars

    @table_columns = {sort: :strain_number, others: [:date_entered, :entered_by, :species, :strainalias]}
    @controller = self.class
    @table_objects = @yeaststrains

  end

end

