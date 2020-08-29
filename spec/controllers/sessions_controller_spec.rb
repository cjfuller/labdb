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
