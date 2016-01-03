class ApiController < ApplicationController

  def fetch
    cls = params[:model].classify.constantize
    @obj = cls.find(params[:id])
    render json: @obj.as_resource_def
  end


  def update
    cls = params[:model].classify.constantize
    @obj = cls.find(params[:id])
    filtered_attrs = params.select { |k| cls::Fields.include? k.to_sym }
    @obj.update_attributes(filtered_attrs)
    render json: {}
  end
end
