require 'test_helper'

class UsersControllerTest < ActionController::TestCase
  setup do
    log_in(@request.session)
    @request.env['HTTPS'] = 'on'
    @user = users(:one)
  end

  test "should not get index" do
    assert_raises_subcl(StandardError) { get :index }
  end

  test "should not get new" do
    assert_raises_subcl(StandardError) { get :new }
    assert_response :success
  end

  test "should not create user" do
    assert_raises_subcl(StandardError) do
      post :create, user: { name: @user.name, provider: @user.provider, uid: @user.uid }
    end
  end

  test "should not show user" do
    assert_raises_subcl(StandardError) { get :show, id: @user } 
  end

  test "should not get edit" do
    assert_raises_subcl(StandardError) { get :edit, id: @user }
  end

  test "should not update user" do
    assert_raises_subcl(StandardError) { put :update, id: @user, user: { name: @user.name, provider: @user.provider, uid: @user.uid } }
  end

  test "should not destroy user" do
    assert_raises_subcl(StandardError) do
      delete :destroy, id: @user
    end
  end
end
