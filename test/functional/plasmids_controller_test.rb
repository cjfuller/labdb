require 'test_helper'

class PlasmidsControllerTest < ActionController::TestCase
  setup do
    log_in(@request.session)
    @request.env['HTTPS'] = 'on'
    @plasmid = plasmids(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:plasmids)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create plasmid" do
    assert_difference('Plasmid.count') do
      post :create, plasmid: { antibiotic: @plasmid.antibiotic, concentration: @plasmid.concentration, date_entered: @plasmid.date_entered, description: @plasmid.description, enteredby: @plasmid.enteredby, notebook: @plasmid.notebook, plasmidalias: @plasmid.plasmidalias, plasmidnumber: @plasmid.plasmidnumber, plasmidsize: @plasmid.plasmidsize, sequence: @plasmid.sequence, strainnumbers: @plasmid.strainnumbers, vector: @plasmid.vector, verified: @plasmid.verified }
    end

    assert_redirected_to plasmid_path(assigns(:plasmid))
  end

  test "should show plasmid" do
    get :show, id: @plasmid
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @plasmid
    assert_response :success
  end

  test "should update plasmid" do
    put :update, id: @plasmid, plasmid: { antibiotic: @plasmid.antibiotic, concentration: @plasmid.concentration, date_entered: @plasmid.date_entered, description: @plasmid.description, enteredby: @plasmid.enteredby, notebook: @plasmid.notebook, plasmidalias: @plasmid.plasmidalias, plasmidnumber: @plasmid.plasmidnumber, plasmidsize: @plasmid.plasmidsize, sequence: @plasmid.sequence, strainnumbers: @plasmid.strainnumbers, vector: @plasmid.vector, verified: @plasmid.verified }
    assert_redirected_to plasmid_path(assigns(:plasmid))
  end

  test "should destroy plasmid" do
    assert_difference('Plasmid.count', -1) do
      delete :destroy, id: @plasmid
    end

    assert_redirected_to plasmids_path
  end

  test "should get search" do

    get :search
    assert_response :success

  end

  test "should do regexp search correctly" do

    get :index, plasmid: {plasmidalias: "/CENP/"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 2)

    get :index, plasmid: {plasmidalias: "/CENP-I/"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 1)

    get :index, plasmid: {plasmidalias: "/CENP-F/"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 0)

  end

  test "should do non-regexp search correctly" do

    get :index, plasmid: {plasmidalias: "*CENP*"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 2)

    get :index, plasmid: {plasmidalias: "CENP"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 0)

  end


  test "should do case-insensitive search correctly" do

    get :index, plasmid: {plasmidalias: "/cenp-i/"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 0)

    get :index, plasmid: {plasmidalias: "/cenp-i/i"}

    obj= assigns(:plasmids)

    assert_equal(obj.size, 1)

  end

  test "should do antibiotic search correctly" do

    get :index, plasmid: {carb: "1"}

    obj = assigns(:plasmids)

    assert_equal(obj.size, 2)

    get :index, plasmid: {gent: "1"}

    obj = assigns(:plasmids)

    assert_equal(obj.size, 1)

    get :index, plasmid: {strep: "1"}

    obj = assigns(:plasmids)

    assert_equal(obj.size, 0)

  end

  test "should export to yaml" do

    get :export, exportformat: "yml", id: @plasmid

    assert_nothing_raised do
      YAML.load(response.body)
    end

  end

end
