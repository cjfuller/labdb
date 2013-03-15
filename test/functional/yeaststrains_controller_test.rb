require 'test_helper'

class YeaststrainsControllerTest < ActionController::TestCase
  setup do
    log_in(@request.session)
    @request.env['HTTPS'] = 'on'
    @yeaststrain = yeaststrains(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:yeaststrains)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create yeaststrain" do
    assert_difference('Yeaststrain.count') do
      post :create, yeaststrain: { antibiotic: @yeaststrain.antibiotic, comments: @yeaststrain.comments, date_entered: @yeaststrain.date_entered, entered_by: @yeaststrain.entered_by, genotype: @yeaststrain.genotype, location: @yeaststrain.location, plasmidnumber: @yeaststrain.plasmidnumber, sequence: @yeaststrain.sequence, species: @yeaststrain.species, strain_bkg: @yeaststrain.strain_bkg, strain_number: @yeaststrain.strain_number, strainalias: @yeaststrain.strainalias }
    end

    assert_redirected_to yeaststrain_path(assigns(:yeaststrain))
  end

  test "should show yeaststrain" do
    get :show, id: @yeaststrain
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @yeaststrain
    assert_response :success
  end

  test "should update yeaststrain" do
    put :update, id: @yeaststrain, yeaststrain: { antibiotic: @yeaststrain.antibiotic, comments: @yeaststrain.comments, date_entered: @yeaststrain.date_entered, entered_by: @yeaststrain.entered_by, genotype: @yeaststrain.genotype, location: @yeaststrain.location, plasmidnumber: @yeaststrain.plasmidnumber, sequence: @yeaststrain.sequence, species: @yeaststrain.species, strain_bkg: @yeaststrain.strain_bkg, strain_number: @yeaststrain.strain_number, strainalias: @yeaststrain.strainalias }
    assert_redirected_to yeaststrain_path(assigns(:yeaststrain))
  end

  test "should destroy yeaststrain" do
    assert_difference('Yeaststrain.count', -1) do
      delete :destroy, id: @yeaststrain
    end

    assert_redirected_to yeaststrains_path
  end

  test "should get search" do

    get :search
    assert_response :success

  end

  test "should do regexp search correctly" do

    get :index, yeaststrain: {strainalias: "/leu-/"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 2)

    get :index, yeaststrain: {strainalias: "/ade-/"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 1)

    get :index, yeaststrain: {strainalias: "/lys-/"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 0)

  end

  test "should do non-regexp search correctly" do

    get :index, yeaststrain: {strainalias: "*leu*"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 2)

    get :index, yeaststrain: {strainalias: "leu"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 0)

  end


  test "should do case-insensitive search correctly" do

    get :index, yeaststrain: {strainalias: "/LEU/"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 0)

    get :index, yeaststrain: {strainalias: "/ADE/i"}

    obj= assigns(:yeaststrains)

    assert_equal(obj.size, 1)

  end

  test "should export to yaml" do

    get :export, exportformat: "yml", id: @yeaststrain

    assert_nothing_raised do
      YAML.load(response.body)
    end

  end


end
