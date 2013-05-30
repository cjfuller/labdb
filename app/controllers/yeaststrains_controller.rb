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

  def obj_tag
    model_class.obj_tag
  end

  def self.get_heading(var_name)
    model_class.get_heading(var_name)
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

  def define_table_view_vars

    @table_columns = {sort: :strain_number, others: [:date_entered, :entered_by, :species, :strainalias]}
    @controller = self.class
    @table_objects = @yeaststrains

  end

end

