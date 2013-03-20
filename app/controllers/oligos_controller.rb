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

  generate_standard_controller_actions(self, Oligo, "Oligo")

end
