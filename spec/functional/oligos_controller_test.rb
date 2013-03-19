require 'test_helper'

class OligosControllerTest < ActionController::TestCase
  setup do
    log_in(@request.session)
    @request.env['HTTPS'] = 'on'
    @oligo = oligos(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:oligos)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create oligo" do
    assert_difference('Oligo.count') do
      post :create, oligo: { oligoalias: @oligo.oligoalias, date_entered: @oligo.date_entered, entered_by: @oligo.entered_by, notebook: @oligo.notebook, oligo_number: @oligo.oligo_number, organism: @oligo.organism, purpose: @oligo.purpose, sequence: @oligo.sequence, vendor: @oligo.vendor }
    end

    assert_redirected_to oligo_path(assigns(:oligo))
  end

  test "should show oligo" do
    get :show, id: @oligo
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @oligo
    assert_response :success
  end

  test "should update oligo" do
    put :update, id: @oligo, oligo: { oligoalias: @oligo.oligoalias, date_entered: @oligo.date_entered, entered_by: @oligo.entered_by, notebook: @oligo.notebook, oligo_number: @oligo.oligo_number, organism: @oligo.organism, purpose: @oligo.purpose, sequence: @oligo.sequence, vendor: @oligo.vendor }
    assert_redirected_to oligo_path(assigns(:oligo))
  end

  test "should destroy oligo" do
    assert_difference('Oligo.count', -1) do
      delete :destroy, id: @oligo
    end

    assert_redirected_to oligos_path
  end


  test "should get search" do

    get :search
    assert_response :success

  end

  test "should do regexp search correctly" do

    get :index, oligo: {organism: "/X. laevis/"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 2)

    get :index, oligo: {oligoalias: "/forward/"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 1)

    get :index, oligo: {organism: "/X. tropicalis/"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 0)

  end

  test "should do non-regexp search correctly" do

    get :index, oligo: {organism: "*laevis*"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 2)

    get :index, oligo: {organism: "laevis"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 0)

  end


  test "should do case-insensitive search correctly" do

    get :index, oligo: {oligoalias: "/FORWARD/"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 0)

    get :index, oligo: {oligoalias: "/FORWARD/i"}

    obj= assigns(:oligos)

    assert_equal(obj.size, 1)

  end

  test "should export to yaml" do

    get :export, exportformat: "yml", id: @oligo

    assert_nothing_raised do
      YAML.load(response.body)
    end

  end

end
