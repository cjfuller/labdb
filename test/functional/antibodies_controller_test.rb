require 'test_helper'

class AntibodiesControllerTest < ActionController::TestCase
  setup do
    @antibody = antibodies(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:antibodies)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create antibody" do
    assert_difference('Antibody.count') do
      post :create, antibody: { ab_number: @antibody.ab_number, alias: @antibody.alias, box: @antibody.box, comments: @antibody.comments, entered_by: @antibody.entered_by, fluorophore: @antibody.fluorophore, good_for_if: @antibody.good_for_if, good_for_western: @antibody.good_for_western, host: @antibody.host, label: @antibody.label, vendor: @antibody.vendor }
    end

    assert_redirected_to antibody_path(assigns(:antibody))
  end

  test "should show antibody" do
    get :show, id: @antibody
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @antibody
    assert_response :success
  end

  test "should update antibody" do
    put :update, id: @antibody, antibody: { ab_number: @antibody.ab_number, alias: @antibody.alias, box: @antibody.box, comments: @antibody.comments, entered_by: @antibody.entered_by, fluorophore: @antibody.fluorophore, good_for_if: @antibody.good_for_if, good_for_western: @antibody.good_for_western, host: @antibody.host, label: @antibody.label, vendor: @antibody.vendor }
    assert_redirected_to antibody_path(assigns(:antibody))
  end

  test "should destroy antibody" do
    assert_difference('Antibody.count', -1) do
      delete :destroy, id: @antibody
    end

    assert_redirected_to antibodies_path
  end
end