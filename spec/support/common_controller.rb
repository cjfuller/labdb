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

module CommonControllerSpecs

	def self.extended(base)

		base.class_exec do 

			include Rails.application.routes.url_helpers

			classname = described_class.name.gsub("Controller", "").classify
			plural_class_sym = classname.downcase.pluralize.to_sym
			model_class = classname.constantize
			inst_var_name = "@" + classname.downcase

			fixtures :users, plural_class_sym

			before :each do
				request.env['HTTPS'] = 'on'
				log_in(request.session)
				instance_variable_set(inst_var_name, self.send(plural_class_sym, :one))
			end

			basic_tests

			object_tests model_class

			export_tests model_class, ["yml"]

			navigation model_class

		end

	end

	def basic_tests

		it "should get a new item" do
			get :new
			response.should be_success
		end

		it "should get the search page" do
			get :search
			response.should be_success
		end

	end


	def object_tests(model_class)

		obj_varname = "@" + model_class.to_s.downcase

		before :each do

			@obj = instance_variable_get(obj_varname)
			@fields_hash = {}
			@obj.exportable_fields.each { |f| @fields_hash[f] = @obj.send(f) }

		end

		model_sym = model_class.to_s.downcase.to_sym

		it "should get the index" do

			get :index

			response.should be_success
			assigns(model_class.to_s.pluralize.downcase.to_sym).should_not be_nil

		end

		it "should show the #{model_class.to_s.downcase}" do
			get :show, id: @obj
			response.should be_success
		end

		it "should get the edit page" do
			get :edit, id: @obj
			response.should be_success
		end

		it "should destroy the #{model_class.to_s.downcase}" do

			lambda { delete :destroy, id: @obj }.should change(model_class, :count).by(-1)

			response.should redirect_to(polymorphic_path(model_class))

		end

		it "should create the #{model_class.to_s.downcase}" do

			lambda { post :create, model_sym => @fields_hash }.should change(model_class, :count)

			response.should redirect_to(polymorphic_path(assigns(model_sym)))

		end

		it "should create a #{model_class.to_s.downcase} filled with the appropriate fields" do

			post :create, model_sym => @fields_hash
			created_obj = assigns(model_class.to_s.downcase.to_sym)

			@fields_hash.each_key { |k| created_obj.send(k).should eq @fields_hash[k] }

		end

		it "should update the #{model_class.to_s.downcase}" do
			put :update, id: @obj, model_sym => @fields_hash
			response.should redirect_to(polymorphic_path(assigns(model_sym)))

		end

	end


	def search_tests(opts)

		model_class = opts[:model_class]
		search_field = opts[:search_field]
		count_0_regexp = opts[:count_0_regexp]
		count_1_regexp = opts[:count_1_regexp]
		count_2_regexp = opts[:count_2_regexp]
		non_regexp_exp = opts[:non_regexp_exp]
		ci_exp = opts[:ci_exp]

		type = model_class.to_s.downcase.to_sym
		plural_type = model_class.to_s.pluralize.downcase.to_sym


		describe "regexp search" do

			it "should get all entries matching a given regexp" do

				get :index, type => {search_field => count_2_regexp}

				assigns(plural_type).size.should eq 2

			end

			it "should not get entries not matching a given regexp" do

				get :index, type => {search_field => count_1_regexp}

				assigns(plural_type).size.should eq 1

			end

			it "should get an empty list of entries for a non-matching regexp" do

				get :index, type => {search_field => count_0_regexp}

				assigns(plural_type).should be_empty

			end

		end


		describe "non-regexp search" do

			it "should get all entries matching a given wildcard expression" do

				get :index, type => {search_field => ("*" + non_regexp_exp + "*")}

				assigns(plural_type).size.should eq 2

			end

			it "should not find entries that are not an exact match to an expression without wildcards" do

				get :index, type => {search_field => (non_regexp_exp)}

				assigns(plural_type).should be_empty
			end

		end

		describe "case-insensitive search" do

			it "should do case-sensitive search by default" do

				get :index, type => {search_field => ci_exp}

				assigns(plural_type).should be_empty

			end

			it "should change to case-insensitive with the /i modifier" do

				get :index, type => {search_field => ci_exp + "i"}

				assigns(plural_type).size.should eq 1

			end

		end

	end

	def export_tests(model_class, formats)

		obj_varname = "@" + model_class.to_s.downcase

		formats.each do |f|

			it "should export to #{f}" do

				get :export, { exportformat: f, id: instance_variable_get(obj_varname) }

				response.should be_success

				if f == "yml" then

					lambda{ YAML.load(response.body) }.should_not raise_error

				end

			end

		end

	end

	def navigation(model_class)

		obj_varname = "@" + model_class.to_s.downcase
		plural_type = model_class.to_s.downcase.pluralize

		before :each do

			@obj = instance_variable_get(obj_varname)

		end

		describe "previous / next navigation" do 

			it "should get the correct item for the next action" do

				get :next, { id: @obj }

				response.should redirect_to self.send(plural_type, :two)

			end


			it "should get the correct item for the previous action" do

				get :previous, { id: self.send(plural_type, :two) }

				response.should redirect_to @obj

			end

		end


	end

end
