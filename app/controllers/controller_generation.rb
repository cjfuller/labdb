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

def generate_standard_controller_actions(controller, model_class_in, text_in)

	controller.class_exec(model_class_in, text_in) do |model_class, text|

		@model_class = model_class
		@text = text

		class << self
			attr_accessor :text, :model_class
		end

		def type
			model_class.to_s.downcase
		end

		def model_class
			self.class.model_class
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


		def index

			define_ui_variables(status_text: self.class.text.pluralize, context_specific_buttons: "shared/top_pagination_buttons")

			page_size = 100
			page = params[:page] or 1

			if params.has_key?(type) then
				@objs = process_search_query(params[type], model_class)
				@objs = Kaminari.paginate_array(@objs).page(page).per(page_size)
				@search_id = find_current_search.id
			elsif valid_search_requested? then
				@search_id = find_current_search.id
				@objs = model_class.find(find_current_search.loaded_result.keys)
				@objs.reverse! if reverse_sorted?
				@objs = Kaminari.paginate_array(@objs).page(page).per(page_size)
			else
				if params[:id_for_page] and not params[:page] then
					page = index_page_number_for_id(params[:id_for_page], page_size)
				end
				@objs = model_class.order(index_order).page(page).per(page_size)
			end


			instance_variable_set("@" + type.pluralize, @objs)

			define_table_view_vars
			define_sort_direction

			respond_to do |format|
				format.html
				format.json { render json: @objs }
			end

		end

		def show
			@obj = model_class.find(params[:id])
			@search_id = params[:search_id]

			preprocess_model_object(@obj)

			instance_variable_set("@" + type, @obj)

			define_ui_variables(status_text: "#{obj_tag} #{@obj.number_field}", context_specific_buttons: "shared/top_editing_buttons", obj: @obj, readonly: true)

			respond_to do |format|
				format.html
				format.json { render json: @obj }
			end
		end

		def new

			if params[:id] then
				@obj = model_class.find(params[:id]).dup
			else
				@obj = model_class.new()
			end

			instance_variable_set("@" + type, @obj)

			define_ui_variables(status_text: "New #{self.class.text.downcase}", readonly: false, submit_text: "Create #{self.class.text.downcase}")

			generate_date(@obj)
			generate_name(@obj)
			@obj.send(@obj.number_field_name.to_s + "=", generate_object_number(model_class, @obj.number_field_name))

			respond_to do |format|
				format.html # new.html.erb
				format.json { render json: @obj }
			end
		end

		def edit
			@obj = model_class.find(params[:id])

			preprocess_model_object(@obj)

			instance_variable_set("@" + type, @obj)

			define_ui_variables(status_text: "Editing #{obj_tag} #{@obj.number_field}", context_specific_buttons: "shared/top_editing_buttons", obj: @obj, readonly: false, submit_text: "Update #{self.class.text.downcase}")
		end

		def create

			@obj = model_class.new(params[type.to_sym])
			instance_variable_set("@" + type, @obj)

			respond_to do |format|
				if @obj.save
					format.html { redirect_to @obj, notice: '#{self.class.text} was successfully created.' }
					format.json { render json: @obj, status: :created, location: @obj }
				else
					format.html { render action: "new" }
					format.json { render json: @obj.errors, status: :unprocessable_entity }
				end
			end
		end

		def update
			@obj = model_class.find(params[:id])
			instance_variable_set("@" + type, @obj)

			respond_to do |format|
				if @obj.update_attributes(params[type])
					format.html { redirect_to @obj, notice: 'obj was successfully updated.' }
					format.json { head :no_content }
				else
					format.html { render action: "edit" }
					format.json { render json: @obj.errors, status: :unprocessable_entity }
				end
			end
		end

		# DELETE /objs/1
		# DELETE /objs/1.json
		def destroy
			@obj = model_class.find(params[:id])
			instance_variable_set("@" + type, @obj)
			@obj.destroy

			respond_to do |format|
				format.html { redirect_to polymorphic_url(model_class) }
				format.json { head :no_content }
			end
		end

		def search
			@obj = model_class.new
			instance_variable_set("@" + type, @obj)

			define_ui_variables(status_text: "Searching #{self.class.text.downcase.pluralize}", obj: @obj, readonly: false, submit_text: "Search")

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

		def next

			obj = model_class.find(params[:id])
			num = obj.number_field.to_i

			next_obj = nil

			if valid_search_requested? then
				objs = find_current_search.loaded_result
				next_id = objs.keys.select { |k| objs[k].to_i > num }.first
				next_obj = model_class.find(next_id) unless next_id.nil?
			else
				next_obj = model_class.where("#{obj.number_field_name.to_s} > ?", obj.number_field).order("#{obj.number_field_name.to_s} ASC").limit(1).first
			end

			handle_next_previous_redirection(next_obj, obj)

		end

		def previous

			obj = model_class.find(params[:id])
			num = obj.number_field.to_i

			next_obj = nil

			if valid_search_requested? then
				objs = find_current_search.loaded_result
				next_id = objs.keys.select { |k| objs[k].to_i < num }.last
				next_obj = model_class.find(next_id) unless next_id.nil?
			else
				next_obj = model_class.where("#{obj.number_field_name.to_s} < ?", obj.number_field).order("#{obj.number_field_name.to_s} DESC").limit(1).first
			end

			handle_next_previous_redirection(next_obj, obj)

		end

	end

end

