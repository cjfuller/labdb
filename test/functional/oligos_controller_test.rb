require 'test_helper'

class OligosControllerTest < ActionController::TestCase
  setup do
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
      post :create, oligo: { alias: @oligo.alias, date_entered: @oligo.date_entered, entered_by: @oligo.entered_by, notebook: @oligo.notebook, oligo_number: @oligo.oligo_number, organism: @oligo.organism, purpose: @oligo.purpose, sequence: @oligo.sequence, vendor: @oligo.vendor }
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
    put :update, id: @oligo, oligo: { alias: @oligo.alias, date_entered: @oligo.date_entered, entered_by: @oligo.entered_by, notebook: @oligo.notebook, oligo_number: @oligo.oligo_number, organism: @oligo.organism, purpose: @oligo.purpose, sequence: @oligo.sequence, vendor: @oligo.vendor }
    assert_redirected_to oligo_path(assigns(:oligo))
  end

  test "should destroy oligo" do
    assert_difference('Oligo.count', -1) do
      delete :destroy, id: @oligo
    end

    assert_redirected_to oligos_path
  end
end