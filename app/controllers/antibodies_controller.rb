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

class AntibodiesController < ApplicationController

  include StandardActions

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

  def self.model_class
    Antibody
  end

  def self.text
    "Antibody"
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

    @table_columns = {sort: :ab_number, others: [:date_entered, :entered_by, :alias, :host]}
    @controller = self.class
    @table_objects = @antibodies

  end

end
