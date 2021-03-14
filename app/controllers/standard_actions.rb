module StandardActions
  def type
    model_class.to_s.downcase
  end

  def model_class
    self.class.model_class
  end

  def define_ui_variables(params)
    params[:model_class] = model_class
    params[:search_path] = search_path
    super(params)
  end

  def reverse_sorted?
    (sort_order == "DESC")
  end

  def define_sort_direction
    @reverse_sorted = reverse_sorted?
  end

  def sort_order
    if params[:sort_order] == "ASC"
      "ASC"
    else
      "DESC"
    end
  end

  def index_order
    "#{model_class.number_field_name} #{sort_order}"
  end

  def index_page_number_for_id(id, page_size)
    objs = model_class.order(index_order)
    ind = objs.find_index { |obj| obj.id.to_i == id.to_i }
    ind / page_size + 1
  end

  def index_all(page_size, start_id)
    if sort_order == "DESC"
      @objs = model_class.order(index_order).where(model_class.number_field_name => 0..start_id).limit(page_size)
    else
      @objs = model_class.order(index_order).where(model_class.number_field_name => start_id..(2 ** 31 - 1)).limit(page_size)
    end
  end

  def index
    page_size = 100
    start_id = (params[:start] or ((sort_order == "DESC") and 2 ** 31 - 1) or 0).to_i
    index_all(page_size, start_id)

    instance_variable_set("@" + type.pluralize, @objs)

    define_table_view_vars
    define_sort_direction

    resource_name = if self.class.respond_to? :resource_name
        self.class.resource_name
      else
        type
      end

    @content_json = JSON.generate({
      type: "collection",
      resourcePath: "/" + resource_name.pluralize.downcase,
      items: @objs.map(&:as_resource_def),
      objectType: resource_name.pluralize,
      numberFieldName: model_class.number_field_name,
    })
    @search_results = JSON.generate([])
    @user_name = current_user.name
    @user_auth = auth_scope
    render "layouts/application.html.haml"
  end

  def show
    @obj = model_class.find(params[:id])
    @search_id = params[:search_id]

    instance_variable_set("@" + type, @obj)

    define_ui_variables(status_text: "#{obj_tag} #{@obj.number_field}", context_specific_buttons: "shared/top_editing_buttons", obj: @obj, readonly: true)
    @content_json = @obj.as_json.html_safe
    @user_name = current_user.name
    @user_auth = auth_scope
    @search_results = JSON.generate([])
    render "layouts/application.html.haml"
  end

  def auto_fill_generated_fields
    generate_date(@obj)
    generate_name(@obj)
    @obj.send(@obj.number_field_name.to_s + "=", generate_object_number(model_class, @obj.number_field_name))
  end

  def search
    @obj = model_class.new
    instance_variable_set("@" + type, @obj)

    define_ui_variables(status_text: "Searching #{self.class.text.downcase.pluralize}", obj: @obj, readonly: false, submit_text: "Search", context_specific_buttons: "shared/search_reference")

    respond_to do |format|
      format.html
      format.json { render json: @obj }
    end
  end

  def export
    @obj = model_class.find(params[:id])
    instance_variable_set("@" + type, @obj)
    do_export(@obj)
  end

  def handle_next_previous_redirection(next_obj, obj)
    params_hash = {}

    if next_obj
      self.status = 302
      self.location = polymorphic_path(next_obj, params_hash)
      self.response_body = ""
    else
      self.status = 302
      self.location = polymorphic_path(obj, params_hash)
      self.response_body = ""
    end
  end

  def find_next_or_previous_obj(direction)
    num_comparators = { next: :>, previous: :< }
    sort_direction = { next: "ASC", previous: "DESC" }
    obj = model_class.find(params[:id])
    num = obj.number_field.to_i
    next_obj = nil

    next_obj = model_class.where("#{obj.number_field_name.to_s} #{num_comparators[direction]} ?", obj.number_field).order("#{obj.number_field_name.to_s} #{sort_direction[direction]}").limit(1).first

    handle_next_previous_redirection(next_obj, obj)
  end

  def next
    find_next_or_previous_obj(:next)
  end

  def previous
    find_next_or_previous_obj(:previous)
  end
end
