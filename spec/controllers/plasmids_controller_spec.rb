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

require 'spec_helper'

describe PlasmidsController do 

  extend CommonControllerSpecs

  search_tests model_class: Plasmid, search_field: :plasmidalias, count_0_regexp: "/CENP-F/", count_1_regexp: "/CENP-I/", count_2_regexp: "/CENP/", non_regexp_exp: "CENP", ci_exp: "/cenp-i/"

  describe "antibiotic search" do

    it "should find all plasmids with a given resistance" do 

      get :index, plasmid: {carb: "1"}

      assigns(:plasmids).size.should eq 2

    end

    it "should not find plasmids that do not match a given resistance" do

      get :index, plasmid: {gent: "1"}

      assigns(:plasmids).size.should eq 1

    end

    it "should not find any plasmids for an antibiotic that does not appear in any plasmid" do 

      get :index, plasmid: {strep: "1"}

      assigns(:plasmids).should be_empty

    end

   end

   describe "antibiotic handling" do 

    it "should display antibiotics correctly" do

      get :show, id: plasmids(:one)

      assigns(:plasmid)[:antibiotic].should eq "carb,kan,gent"
      assigns(:plasmid)[:carb].should eq "1"
      assigns(:plasmid)[:kan].should eq "1"
      assigns(:plasmid)[:gent].should eq "1"
      assigns(:plasmid)[:chlor].should eq "0"

    end

    it "should update antibiotics correctly on edit" do 

      get :edit, id: plasmids(:one)
      plas = assigns(:plasmid)

      params_hash = model_to_hash(plas)

      params_hash[:carb] = "0"

      put :update, id: plasmids(:one), plasmid: params_hash

      assigns(:plasmid)[:antibiotic].should eq "kan, gent"


    end

    it "should calculate antibiotics correctly on create" do

      plas_new = plasmids(:one)
      params_hash = model_to_hash(plas_new)
      params_hash[:antibiotic] = ""
      params_hash[:kan] = "1"
      [:carb, :chlor, :gent, :strep, :tet].each { |a| params_hash[a] = "0" }

      post :create, plasmid: params_hash

      assigns(:plasmid)[:antibiotic].should eq "kan"

    end

  end

end
