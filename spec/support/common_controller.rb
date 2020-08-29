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
        request.env["HTTPS"] = "on"
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
      it "should get the search page" do
        get :search
        response.should be_success
      end

      it "url_for should match the correct resource" do
        plural_class = self.class.plural_class_sym.to_s
        @obj = instance_variable_get(obj_varname = "@" + model_class.to_s.downcase)
        url_for(controller: plural_class, host: "test.host", action: :show, id: @obj.id, protocol: "https").should eq "https://test.host/#{plural_class}/#{@obj.id}"
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

      it "should get the index" do
        get :index
        response.should be_success
        assigns(model_class.to_s.pluralize.downcase.to_sym).should_not be_nil
      end

      it "should show the #{model_class.to_s.downcase}" do
        get :show, id: @obj
        response.should be_success
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

          if f == "yml"
            lambda { YAML.load(response.body) }.should_not raise_error
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
