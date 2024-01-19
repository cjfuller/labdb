class DinosController < ApplicationController
  include StandardActions

  def self.model_class
    Dino
  end

  def obj_tag
    model_class.obj_tag
  end

  def self.resource_name
    "dino"
  end

  def define_table_view_vars
    @table_columns = {sort: :number, others: [:created_at, :entered_by, :alias]}
    @controller = self.class
    @table_objects = @dinos
  end
end
