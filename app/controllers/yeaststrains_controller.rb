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
    @table_columns = { sort: :strain_number, others: [:date_entered, :entered_by, :species, :strainalias] }
    @controller = self.class
    @table_objects = @yeaststrains
  end
end
