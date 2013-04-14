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

describe BacteriaController do 
	
	extend CommonControllerSpecs

	search_tests model_class: Bacterium, search_field: :species_bkg, count_0_regexp: "/Bacillus/", count_1_regexp: "/DH5a/", count_2_regexp: "/E. coli/", non_regexp_exp: "E. coli", ci_exp: "/dh5a/"


	describe "creation from plasmid" do

		COPIED_FIELDS = {plasmidnumber: :plasmid_number, enteredby: :entered_by, notebook: :notebook, description: :comments, plasmidalias: :strainalias}

		fixtures :plasmids

		before :each do 

			@orig_n_strains = Bacterium.all.size
			@plas = plasmids(:one)
			post :create_from_plasmid, plasmid_id: @plas.id
			@plas = Plasmid.find(@plas.id)
			@new_id = /bacteria\/(\d+)\/edit/.match(response.redirect_url)[1]
		end

		it "should have created a new plasmid" do
			Bacterium.all.size.should eq (@orig_n_strains + 1)
		end

		it "should have redirected to the edit action of the new strain" do
			bact = Bacterium.all.sort { |e1, e2| e1.strain_number.to_i <=> e2.strain_number.to_i }.last
			response.should redirect_to polymorphic_path(bact, action: :edit)
		end

		it "should automatically fill only relevant fields from the plasmid" do
			get :edit, id: @new_id
			bact = assigns(:bacterium)
			COPIED_FIELDS.each do |plas, str|
				bact.send(str).to_s.should eq @plas.send(plas).to_s
			end
			bact.genotype.should be_nil
			bact.species_bkg.should be_nil
		end

		it "should redirect to the new plasmid page if no plasmid is supplied" do
			post :create_from_plasmid, plasmid_id: nil
			response.should redirect_to controller: 'bacteria', action: :new
		end

		it "should append the new strain number to the strain numbers for the referring plasmid" do
			get :edit, id: @new_id
			bact = assigns(:bacterium)
			@plas.strainnumbers.match(bact.strain_number.to_s).should be_true
		end

	end

end
