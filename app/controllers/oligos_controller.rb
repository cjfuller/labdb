class OligosController < ApplicationController
  include StandardActions

  def self.get_heading(var_name)
    model_class.get_heading(var_name)
  end

  def obj_tag
    model_class.obj_tag
  end

  def self.model_class
    Oligo
  end

  def self.text
    "Oligo"
  end

  def search_path
    "/oligos/search"
  end

  def define_table_view_vars
    @table_columns = { sort: :oligo_number, others: [:date_entered, :entered_by, :oligoalias] }
    @controller = self.class
    @table_objects = @oligos
  end
end
