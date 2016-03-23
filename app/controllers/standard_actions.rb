#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

module StandardActions

  def type
    model_class.to_s.downcase
  end

  def model_class
    self.class.model_class
  end

  def define_ui_variables(params)
    params[:model_class]= model_class
    params[:search_path]= search_path
    super(params)
  end

  def reverse_sorted?
    (sort_order == "DESC")
  end

  def define_sort_direction
    @reverse_sorted = reverse_sorted?
  end

  def sort_order
    order = "DESC"
    if params[:sort_order] == "ASC" then
      order = "ASC"
    end
    order
  end

  def index_order
    "#{model_class.number_field_name} #{sort_order}"
  end

  def index_page_number_for_id(id, page_size)
    objs = model_class.order(index_order)
    ind = objs.find_index { |obj| obj.id.to_i == id.to_i }
    ind/page_size + 1
  end

  def index_with_new_search(page_size, page)
    @objs = process_search_query(params[type], model_class)
    @objs = Kaminari.paginate_array(@objs).page(page).per(page_size)
    @search_id = find_current_search.id
  end

  def index_with_stored_search(page_size, page)
    @search_id = find_current_search.id
    result = find_current_search.loaded_result
    @objs = model_class.find(result.keys).sort { |e1, e2| result[e1.id].to_i <=> result[e2.id].to_i }
    @objs.reverse! if reverse_sorted?
    @objs = Kaminari.paginate_array(@objs).page(page).per(page_size)
  end

  def index_all(page_size, start_id)
    if sort_order == "DESC" then
      @objs = model_class.order(index_order).select { |o| o.send(o.number_field_name) <= start_id}.take(page_size)
    else
      @objs = model_class.order(index_order).select { |o| o.send(o.number_field_name) >= start_id}.take(page_size)
    end
  end

  def index
    page_size = 100
    start_id = (params[:start] or (
      (sort_order == "DESC") and 2**64 - 1) or 0).to_i
    # TODO: use start_id for searches too
    page = 1
    if params.has_key?(type) then
      index_with_new_search(page_size, page)
    elsif valid_search_requested? then
      index_with_stored_search(page_size, page)
    else
      index_all(page_size, start_id)
    end

    instance_variable_set("@" + type.pluralize, @objs)

    if @objs.size == 1 then
      redirect_to @objs[0] and return
    end
    define_table_view_vars
    define_sort_direction

    @content_json = JSON.generate({
            type: "collection",
            resourcePath: "/" + type.pluralize.downcase,
            items: @objs.map(&:as_resource_def),
            numberFieldName: @objs[0].number_field_name
    })
    @search_results = JSON.generate([]);
    @user_name = current_user.name
    @user_auth = auth_scope
    render 'layouts/application.html.haml'
  end

  def show
    @obj = model_class.find(params[:id])
    @search_id = params[:search_id]

    #preprocess_model_object(@obj)

    instance_variable_set("@" + type, @obj)

    define_ui_variables(status_text: "#{obj_tag} #{@obj.number_field}", context_specific_buttons: "shared/top_editing_buttons", obj: @obj, readonly: true)
    @content_json = @obj.as_json.html_safe
    @user_name = current_user.name
    @user_auth = auth_scope
    @search_results = JSON.generate([]);
    render 'layouts/application.html.haml'
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
    params_hash[:search_id] = params[:search_id] if valid_search_requested?

    if next_obj then
      redirect_to polymorphic_path(next_obj, params_hash)
    else
      redirect_to polymorphic_path(obj, params_hash)
    end

  end

  def find_next_or_previous_obj(direction)
    num_comparators = {next: :>, previous: :<}
    sort_direction = {next: "ASC", previous: "DESC"}
    obj = model_class.find(params[:id])
    num = obj.number_field.to_i
    next_obj = nil

    if valid_search_requested? then
      objs = find_current_search.loaded_result
      ids = objs.keys.select { |k| objs[k].to_i.send(num_comparators[direction], num) }.sort { |e1, e2| objs[e1].to_i <=> objs[e2].to_i }
      ids.reverse! if direction == :previous
      next_id = ids.first
      next_obj = model_class.find(next_id) unless next_id.nil?
    else
      next_obj = model_class.where("#{obj.number_field_name.to_s} #{num_comparators[direction]} ?", obj.number_field).order("#{obj.number_field_name.to_s} #{sort_direction[direction]}").limit(1).first
    end

    handle_next_previous_redirection(next_obj, obj)
  end

  def next
    find_next_or_previous_obj(:next)
  end

  def previous
    find_next_or_previous_obj(:previous)
  end

end


