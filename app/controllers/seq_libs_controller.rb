
class SeqLibsController < ApplicationController
  include StandardActions

  def self.model_class
    SeqLib
  end

  def obj_tag
    model_class.obj_tag
  end

  def self.resource_name
    "seq_lib"
  end

  def define_table_view_vars
    @table_columns = {sort: :number, others: [:created_at, :entered_by, :alias]}
    @controller = self.class
    @table_objects = @seq_libs
  end
end
