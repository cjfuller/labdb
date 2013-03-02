require 'test_helper'

class BacteriaControllerTest < ActionController::TestCase
  setup do
    @bacterium = bacteria(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:bacteria)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create bacterium" do
    assert_difference('Bacterium.count') do
      post :create, bacterium: { comments: @bacterium.comments, date_entered: @bacterium.date_entered, entered_by: @bacterium.entered_by, genotype: @bacterium.genotype, notebook: @bacterium.notebook, plasmid_number: @bacterium.plasmid_number, species_bkg: @bacterium.species_bkg, strain_number: @bacterium.strain_number }
    end

    assert_redirected_to bacterium_path(assigns(:bacterium))
  end

  test "should show bacterium" do
    get :show, id: @bacterium
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @bacterium
    assert_response :success
  end

  test "should update bacterium" do
    put :update, id: @bacterium, bacterium: { comments: @bacterium.comments, date_entered: @bacterium.date_entered, entered_by: @bacterium.entered_by, genotype: @bacterium.genotype, notebook: @bacterium.notebook, plasmid_number: @bacterium.plasmid_number, species_bkg: @bacterium.species_bkg, strain_number: @bacterium.strain_number }
    assert_redirected_to bacterium_path(assigns(:bacterium))
  end

  test "should destroy bacterium" do
    assert_difference('Bacterium.count', -1) do
      delete :destroy, id: @bacterium
    end

    assert_redirected_to bacteria_path
  end
end
