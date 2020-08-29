class SamplesController < ApplicationController
  include StandardActions

  def obj_tag
    self.class.model_class.obj_tag
  end

  def self.get_heading(var_name)
    model_class.get_heading(var_name)
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
    @table_columns = { sort: :sample_number, others: [:date_entered, :entered_by, :sample_alias, :sample_type] }
    @controller = self.class
    @table_objects = @samples
  end
end
