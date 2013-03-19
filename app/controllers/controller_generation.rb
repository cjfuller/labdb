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
			model_class.to_s.downcase.to_sym
		end

		def model_class
			self.class.model_class
		end

		def index

			puts model_class.all.map(&:inspect)

			define_ui_variables(status_text: self.class.text.pluralize, context_specific_buttons: "shared/top_pagination_buttons")

			page_size = 250
			params[:page] = 1 unless params[:page]

			if params.has_key?(type) then
				@objs = process_search_query(params[type], model_class)
				page_size = @objs.size
			else
				@objs = model_class.all
			end

			@objs.sort! { |e0, e1| e0.number_field.to_i <=> e1.number_field.to_i }

			@objs = Kaminari.paginate_array(@objs).page(params[:page]).per(page_size)

			define_table_view_vars

			respond_to do |format|
				format.html
				format.json { render json: @objs }
			end

		end

		def show
			@obj = model_class.find(params[:id])

			define_ui_variables(status_text: "#{obj_tag} #{@obj.number_field}", context_specific_buttons: "shared/top_editing_buttons", obj: @obj, readonly: true)

			respond_to do |format|
				format.html
				format.json { render json: @obj }
			end
		end

		def new
			@obj = model_class.new

			define_ui_variables(status_text: "New #{self.class.text.downcase}", readonly: false, submit_text: "Create #{self.class.text.downcase}")

			generate_date(@obj)
			generate_name(@obj)
			@obj.strain_number = generate_object_number(model_class, :strain_number)

			respond_to do |format|
				format.html # new.html.erb
				format.json { render json: @obj }
			end
		end

		def edit
			@obj = model_class.find(params[:id])

			define_ui_variables(status_text: "Editing #{obj_tag} #{@obj.number_field}", context_specific_buttons: "shared/top_editing_buttons", obj: @obj, readonly: false, submit_text: "Update #{self.class.text.downcase}")
		end

		def create
			@obj = model_class.new(params[:obj])

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
			@obj.destroy

			respond_to do |format|
				format.html { redirect_to polymorphic_url(model_class) }
				format.json { head :no_content }
			end
		end

		def search
			@obj = model_class.new

			define_ui_variables(status_text: "Searching #{self.class.text.downcase.pluralize}", obj: @obj, readonly: false, submit_text: "Search")

			respond_to do |format|
				format.html
				format.json { render json: @obj }
			end
		end

		def export

			@obj = model_class.find(params[:id])

			do_export(@obj)

		end

	end

end


