require "spec_helper"

describe SessionsController do
  fixtures :users

  before :each do
    request.env["HTTPS"] = "on"
    log_in(request.session)
  end

  it "should raise an error on get new" do
    lambda { get :new }.should raise_error
  end

  it "should get create" do
    request.env["omniauth.auth"] = { provider: "google", uid: "0001", "info" => { "email" => "example@gmail.com", "name" => "First Last" } }
    get :create, provider: "google"
    response.should redirect_to "/"
  end

  it "should redirect on failure" do
    get :failure
    response.should redirect_to "/"
  end
end
