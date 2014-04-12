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

  actions = {index: :get, toggle_auth_read: :put, toggle_auth_write: :put, toggle_auth_admin: :put, new: :get, create: :post, edit: :get, update: :put, destroy: :delete}
  needs_id = [:edit, :toggle_auth_read, :toggle_auth_write, :toggle_auth_admin, :update, :destroy]

  before :each do
    request.env['HTTPS'] = 'on'
  end

  context "admin user" do
    before :each do
      log_in_admin(request.session)
    end

    it "should get all users on index" do
      get :index
      assigns(:users).size.should eq 4
    end

    it "should succeed for get edit" do
      get :edit, id: User.find_by_email('rw@labdb').id
      response.should be_success
    end

    it "should succeed for get new" do
      get :new
      response.should be_success
    end

    needs_id.each do |act|
      it "should fail on #{act} if trying to access the logged in user" do
        send(actions[act], act, id: @controller.curr_user_id)
        response.should redirect_to users_path
      end
    end

    it "should succeed for put update" do
      email = 'rw@labdb'
      new_name = 'test'
      put :update, id: User.find_by_email(email), user: {name: new_name}
      User.find_by_email(email).name.should eq new_name
    end

    it "should succeed for post create" do
      opts = {
        name: 'test',
        email: 'test@labdb',
        notes: ''
      }

      post :create, user: opts
      User.where(email: 'test@labdb').should_not be_empty
    end

    it "should succeed for delete user" do
      email = 'unauthorized@labdb'
      previous_count = User.where(email: email).size
      previous_count.should_not eq 0
      delete :destroy, id: User.where(email: email).first.id
      User.where(email: email).size.should eq previous_count - 1
    end

    [:toggle_auth_admin, :toggle_auth_write, :toggle_auth_read].each do |tog|
      it "should succeed for #{tog}" do
        email = 'rw@labdb'
        user = User.find_by_email(email)
        previous_state = user.send(tog.to_s.gsub("toggle_", ""))
        put tog, id: user.id
        user = User.find_by_email(email)
        user.send(tog.to_s.gsub("toggle_", "")).should eq (not previous_state)
      end
    end
  end

  context "non-admin user" do
    before :each do
      log_in_rw(request.session)
    end

    actions.each do |act, method|
      it "should disallow access for action #{act}" do
        opts = {}
        if needs_id.include? act then
          opts[:id] = 1
        end
        send(method, act, opts)
        response.should redirect_to "/"
      end
    end
  end
end


