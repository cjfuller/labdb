require "net/http"

class Object
  # TODO: seriously, don't do this.
  def update_dynamic_field(field_name_fn_name, value)
    if self.respond_to?(field_name_fn_name)
      fname = self.send(field_name_fn_name)
      self.send(fname.to_s + "=", value)
    elsif self.class.respond_to?(field_name_fn_name)
      fname = self.class.send(field_name_fn_name)
      self.send(fname.to_s + "=", value)
    end
    self
  end
end

class ApiController < ApplicationController
  skip_before_action :require_authorization, only: [:verify, :logout]
  before_action :safeguard_self_modification, only: [:delete, :update]

  def safeguard_self_modification
    return unless params[:id]
    cls = params[:model].classify.constantize
    return unless cls == User
    @user = User.find(params[:id])
    if @user == curr_user_obj
      return head :conflict
    end
  end

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
      numberFieldName: objs[0].number_field_name,
    }
    render json: content_json
  end

  def update
    puts params
    cls = params[:model].classify.constantize
    @obj = cls.find(params[:id])
    filtered_attrs = params.select { |k| cls::Fields.include? k.to_sym }
    @obj.update_attributes(filtered_attrs.permit(cls::Fields))
    render json: {}
  end

  def new_item_setup(obj, should_render: true, should_save: true)
    num = generate_object_number(obj.class, obj.number_field_name)
    obj
      .update_dynamic_field(:number_field_name, num)
      .update_dynamic_field(:owner_field_name, curr_username)
      .update_dynamic_field(:timestamp_field_name, Time.now)

    obj.save if should_save
    render json: obj.as_resource_def if should_render
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

  APP_ID = "146923434465-alq7iagpanjvoag20smuirj0ivdtfldk.apps.googleusercontent.com"

  def verify
    endpoint = URI("https://www.googleapis.com/oauth2/v3/tokeninfo?id_token=#{params[:token]}")
    Net::HTTP.start(endpoint.host, endpoint.port, use_ssl: true) do |http|
      request = Net::HTTP::Get.new endpoint
      response = http.request request
      if response.code == "200"
        info = JSON.parse(response.body)
        if info["aud"].include? APP_ID and info["email_verified"] == "true"
          user = User.find_by_email(info["email"])
          if user
            session[:user_id] = user.email
            return head :no_content
          end
        end
      end
    end
    session[:user_id] = nil
    head :forbidden
  end

  def logout
    session[:user_id] = nil
    head :no_content
  end

  def plasmid_map
    seq = request.body.read
    render json: PlasmidMapping.map_for_plasmid(seq).map(&:to_h)
  end

  def import
    files = params.select { |k, _| k.to_s.starts_with? "file_" }
                  .values
                  .map { |f| JSON.parse(f.read) }

    make_reference = proc do |item|
      "Originally #{item["name"]} from the #{item["database"]}."
    end

    files.each do |f|
      type = f.keys[0]
      item = f.values[0]
      cls = type.constantize
      description = cls.description_field_name.to_s
      item[description] = ((item[description] || "") + "\n\n\n" + make_reference.call(item))
      keys_to_delete = ["database", "name"] + [cls.number_field_name.to_s]
      item.delete_if { |k, _| keys_to_delete.include?(k) }
      obj = cls.new
      new_item_setup(obj, should_render: false, should_save: false)
      obj.update_attributes(item)
      obj.save
    end

    head :no_content
  end
end
