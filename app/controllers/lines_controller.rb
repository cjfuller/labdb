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

class LinesController < ApplicationController

  include StandardActions

  OBJ_TAG = Naming.name_for(Line)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {current_stock_counts: "Stock counts", date_entered: "Date entered", description: "Description", entered_by: "Entered by", line_alias: "Alias", line_number: "#{OBJ_TAG} number", locations: "Locations", notebook: "Notebook", parent_line: "Parent line", plasmid_numbers: "#{Naming.name_for(Plasmid)} numbers", selectable_markers: "Selectable markers", sequence: "Associated sequence", species: "Species"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def self.model_class
    Line
  end

  def self.text
    "Cell line"
  end

  def search_path
    "/lines/search"
  end

  def define_table_view_vars

    @table_columns = {sort: :line_number, others: [:date_entered, :entered_by, :line_alias, :species]}
    @controller = self.class
    @table_objects = @lines

  end

  def update_number

    @obj = Line.find(params[:id])
    loc = params[:location]
    inc = params[:inc].to_i

    inv = @obj.inventory

    inv[loc] += inc

    if inv[loc] <= 0 then
      inv.delete(loc)
    end

    @obj.update_inventory(inv)

    @obj.save

    redirect_to @obj

  end

end
