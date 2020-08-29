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

  before_action :generate_location_params, only: [:create, :update]

  def obj_tag
    self.class.model_class.obj_tag
  end

  def self.get_heading(var_name)
    model_class.get_heading(var_name)
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
    @table_columns = { sort: :line_number, others: [:date_entered, :entered_by, :line_alias, :species] }
    @controller = self.class
    @table_objects = @lines
  end

  def generate_location_params
    table_params = [:location, :count, :clone, :person, :date]
    table_index_sep = "_"

    return unless params.keys.include?(table_params[0].to_s + table_index_sep + "0")

    inv = []

    i = -1

    while i += 1 and params.keys.include?(table_params[0].to_s + table_index_sep + i.to_s)
      next unless params[table_params[0].to_s + table_index_sep + i.to_s].size > 0
      h = {}
      table_params.each { |p| h[p] = params[p.to_s + table_index_sep + i.to_s] }
      inv << Line::InventoryItem.from_hash(h)
    end

    obj = nil

    if params.has_key?(:id)
      obj = Line.find(params[:id])
    else
      obj = Line.new
    end

    obj.update_inventory(inv)

    obj.class::InvFields.each do |f|
      params[type][f] = obj.send(f)
    end
  end
end
