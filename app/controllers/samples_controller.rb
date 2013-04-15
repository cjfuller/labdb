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

class SamplesController < ApplicationController
  
  include StandardActions

  OBJ_TAG = Naming.name_for(Sample)

  def obj_tag
    OBJ_TAG
  end

  @@headings = {date_entered: "Date entered", depleted: "Sample depleted?", description: "Description", entered_by: "Entered by", linked_items: "Linked to", notebook: "Notebook", sample_alias: "Alias", sample_number: "#{OBJ_TAG} number", sample_type: "Sample type", storage_type: "Storage location"}


  def self.get_heading(var_name)
    @@headings[var_name]
  end

  def self.model_class
    Sample
  end

  def self.text
    "Sample"
  end

  def search_path
    "/samples/search"
  end

  def define_table_view_vars

    @table_columns = {sort: :sample_number, others: [:date_entered, :entered_by, :sample_alias, :sample_type]}
    @controller = self.class
    @table_objects = @samples

  end

end
