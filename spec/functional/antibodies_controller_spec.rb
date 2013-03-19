require 'spec_helper'
require 'yaml'

describe AntibodiesController do

	before :each do
		post 'sessions/create', log_in_opts
		request.env['HTTPS'] = 'on'
		@antibody = antibodies(:one)
	end

	it "should get the index" do
		get :index

		response.should be_success :success
		assigns(:antibodies).should_not be_nil

	end

	it "should get a new item" do
		get :new
		response.should be_success
	end

	it "should create an antibody" do
		Proc.new {
			post :create, antibody: { ab_number: @antibody.ab_number, alias: @antibody.alias, box: @antibody.box, comments: @antibody.comments, entered_by: @antibody.entered_by, fluorophore: @antibody.fluorophore, good_for_if: @antibody.good_for_if, good_for_western: @antibody.good_for_western, host: @antibody.host, label: @antibody.label, vendor: @antibody.vendor }
		}.should_change(Antibody, :count)

		response.should redirect_to(antibody_path(assigns(:antibody)))

	end

	it "should show an antibody" do
		get :show, id: @antibody
		response.should be_success
	end

	it "should get the edit page" do
		get :edit, id: @antibody
		response.should be_success
	end

	it "should update an antibody" do
		put :update, id: @antibody, antibody: { ab_number: @antibody.ab_number, alias: @antibody.alias, box: @antibody.box, comments: @antibody.comments, entered_by: @antibody.entered_by, fluorophore: @antibody.fluorophore, good_for_if: @antibody.good_for_if, good_for_western: @antibody.good_for_western, host: @antibody.host, label: @antibody.label, vendor: @antibody.vendor }

		response.should redirect_to(antibody_path(assigns(:antibody)))

	end

	it "should destroy an antibody" do

		(Proc.new do
			delete :destroy, id: @antibody
		end).should_change(Antibody, :count).by(-1)

		response.should redirect_to(antibodies_path)

	end

	it "should get the search page" do

		get :search
		response.should be_success

	end

	describe "regexp search" do

		it "should get all entries matching a given regexp" do

			get :index, antibody: {alias: "/CENP/"}

			assigns(:antibodies).size.should eq 2

		end

		it "should not get entries not matching a given regexp" do

			get :index, antibody: {alias: "/CENP-C/"}

			assigns(:antibodies).size.should eq 1

		end

		it "should get an empty list of entries for a non-matching regexp" do

			get :index, antibody: {alias: "/CENP-F/"}

			assigns(:antibodies).should be_empty

		end

	end

	describe "non-regexp search" do

		it "should get all entries matching a given wildcard expression" do

			get :index, antibody: {alias: "*CENP*"}

			assigns(:antibodies).size.should eq 2

		end

		it "should not find entries that are not an exact match to an expression without wildcards" do

			get :index, antibody: {alias: "CENP"}

			assigns(:antibodies).should be_empty
		end

	end

	describe "case-insensitive search" do

		it "should do case-sensitive search by default" do

			get :index, antibody: {alias: "/cenp-c/"}

			assigns(:antibodies).should be_empty

		end

		it "should change to case-insensitive with the /i modifier" do

			get :index, antibody: {alias: "/cenp-c/i"}

			assigns(:antibodies).size.should eq 1

		end

	end

	it "should export to yaml" do

		get :export, exportformat: "yml", id: @antibody

		assert_nothing_raised do
			YAML.load(response.body)
		end

	end


end
