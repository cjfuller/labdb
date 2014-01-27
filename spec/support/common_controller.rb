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

      def self.classname
        described_class.name.gsub("Controller", "").classify
      end

      def self.plural_class_sym
        classname.downcase.pluralize.to_sym
      end

      def self.model_class
        classname.constantize
      end

      def self.inst_var_name
        "@" + classname.downcase
      end

      fixtures :users, plural_class_sym

      before :each do
        request.env['HTTPS'] = 'on'
        log_in(request.session)
        instance_variable_set(base.inst_var_name, self.send(base.plural_class_sym, :one))
      end

      basic_tests model_class
      object_tests model_class
      export_tests model_class, ["yml"]
      navigation model_class
    end

  end

  def basic_tests(model_class)
    describe "Basic controller actions" do
      it "should get a new item" do
        get :new
        response.should be_success
      end

      it "should get a new item with duplicate" do
        @obj = instance_variable_get(obj_varname = "@" + model_class.to_s.downcase)
        get :new, id: @obj
        response.should be_success
      end

      it "should get the search page" do
        get :search
        response.should be_success
      end

      it "url_for should match the correct resource" do
        plural_class = self.class.plural_class_sym.to_s
        @obj = instance_variable_get(obj_varname = "@" + model_class.to_s.downcase)
        url_for(controller: plural_class, host: "test.host", action: :show, id: @obj.id, protocol: 'https').should eq "https://test.host/#{plural_class}/#{@obj.id}"
      end
    end
  end


  def object_tests(model_class)
    describe "Basic object testing" do 
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

    describe "search redirection" do
      it "should redirect to an object if it's the only one found" do
        get :index, type => {search_field => count_1_regexp}
        response.should redirect_to assigns(plural_type)[0]
      end

      it "should not redirect to an object if multiple are found" do
        get :index, type => {search_field => count_2_regexp}
        response.should be_success
      end
    end
    
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

    describe "navigation within search" do 

      context "valid search provided" do

        before :each do 
          get :index, type => {search_field => count_1_regexp}
          @s = assigns(:search_id)
          @search_obj = assigns(plural_type)[0]
        end

        it "should limit previous navigation to within the search results" do
          get :previous, id: @search_obj.id, search_id: @s
          response.should redirect_to(action: :show, id: @search_obj.id, search_id: @s)
        end

        it "should limit next navigation to within the search results" do
          get :next, id: @search_obj.id, search_id: @s
          response.should redirect_to(action: :show, id: @search_obj.id, search_id: @s)
        end
      end

      context "invalid search provided" do

        before :each do 
          get :index, type => {self.send(plural_type, :one).number_field_name => 1.to_s}
          @s = assigns(:search_id)
          @search_obj = assigns(plural_type)[0]
        end

        context "expired search" do

          before :each do 
            s = Search.find(@s)
            s.expires = (Time.now - 10).to_date
            s.save
          end

          it "should limit previous/next navigation to within the search results when at the first object" do
            get :previous, id: @search_obj.id, search_id: @s
            response.should redirect_to(action: :show, id: @search_obj.id)
          end

          it "should not limit previous/next navigation to within the search results" do
            get :next, id: @search_obj.id, search_id: @s
            response.should redirect_to(action: :show, id: self.send(plural_type, :two).id)
          end

          it "should be an invalid search if the search time has expired" do
            get :show, id: @search_obj.id, search_id: @s
            controller.valid_search_requested?.should_not be_true
          end
        end

        context "not recent search" do

          before :each do 
            get :index, type => {self.send(plural_type, :one).number_field_name => 2.to_s}
            @s_2 = assigns(:search_id)
            @search_obj_2 = assigns(plural_type)[0]
          end

          it "should be an invalid search if it's not the last search a user performed" do 
            get :show, id: @search_obj_2.id, search_id: @s
            controller.valid_search_requested?.should_not be_true
          end
        end
      end
    end
  end

  def export_tests(model_class, formats)
    obj_varname = "@" + model_class.to_s.downcase
    describe "Export" do
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

