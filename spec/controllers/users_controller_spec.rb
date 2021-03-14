describe UsersController do
  fixtures :users

  actions = { index: :get }

  before :each do
    request.env["HTTPS"] = "on"
  end

  context "admin user" do
    before :each do
      log_in_admin(request.session)
    end

    it "should get all users on index" do
      get :index
      assigns(:users).size.should eq 4
    end
  end

  context "non-admin user" do
    before :each do
      log_in_rw(request.session)
    end

    actions.each do |act, method|
      it "should disallow access for action #{act}" do
        opts = {}
        send(method, act, opts)
        response.should have_http_status 403
      end
    end
  end
end
