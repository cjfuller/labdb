require 'test_helper'

class SessionsControllerTest < ActionController::TestCase
  setup do
    log_in(@request.session)
    @request.env['HTTPS'] = 'on'
  end
  
  test "should not get new" do
    assert_raises_subcl(StandardError) { get :new }
  end

  test "should get create" do
    @request.env['omniauth.auth']= {provider: 'google', uid: '0001', 'info' => {'email' => 'example@gmail.com', 'name' => 'First Last'}}
    get :create
    assert_redirected_to "/"
  end

  test "should redirect on failure" do
    get :failure
    assert_redirected_to "/"
  end

end
