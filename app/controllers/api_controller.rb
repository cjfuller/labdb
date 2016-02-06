class Object
  # TODO: seriously, don't do this.
  def update_dynamic_field(field_name_fn_name, value)
    if self.respond_to?(field_name_fn_name) then
      fname = self.send(field_name_fn_name)
      self.send(fname.to_s + "=", value)
    end
    self
  end
end

class ApiController < ApplicationController

  def fetch
    cls = params[:model].classify.constantize
    @obj = cls.find(params[:id])
    render json: @obj.as_resource_def
  end

  def list
    # TODO: use the params so that we can control offset, etc.
    cls = params[:model].classify.constantize
    objs = cls.order("id DESC").take(100)
    content_json = {
      type: "collection",
      resourcePath: "/" + params[:model].pluralize.downcase,
      items: objs.map(&:as_resource_def),
      numberFieldName: objs[0].number_field_name
    }
    render json: content_json
  end

  def update
    cls = params[:model].classify.constantize
    @obj = cls.find(params[:id])
    filtered_attrs = params.select { |k| cls::Fields.include? k.to_sym }
    @obj.update_attributes(filtered_attrs)
    render json: {}
  end

  def new_item_setup(obj)
    num = generate_object_number(obj.class, obj.number_field_name)
    obj
      .update_dynamic_field(:number_field_name, num)
      .update_dynamic_field(:owner_field_name, curr_username)
      .update_dynamic_field(:timestamp_field_name, Time.now)

    obj.save
    render json: obj.as_resource_def
  end

  def new
    cls = params[:model].classify.constantize
    @obj = cls.new
    new_item_setup(@obj)
  end

  def copy
    cls = params[:model].classify.constantize
    obj = cls.find(params[:id]).dup
    new_item_setup(obj)
  end

  def delete
    cls = params[:model].classify.constantize
    @obj = cls.find(params[:id])
    @obj.delete
    head :no_content
  end
end
