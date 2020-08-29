#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

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
