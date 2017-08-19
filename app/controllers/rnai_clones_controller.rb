class RnaiClonesController < ApplicationController
  include StandardActions

  def self.model_class
    RnaiClone
  end

  def obj_tag
    model_class.obj_tag
  end

  def self.resource_name
    'rnai_clone'
  end

  def define_table_view_vars
    @table_columns = {sort: :number, others: [:created_at, :entered_by, :alias]}
    @controller = self.class
    @table_objects = @rnai_clones
  end
end