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

require 'spec_helper'

describe UsersController do

  fixtures :users

  before :each do
    request.env['HTTPS'] = 'on'
    log_in(request.session)
    @user = users(:one)
  end

  it "should raise an error on get index" do
    lambda { get :index }.should raise_error
  end

  it "should raise an error on get new" do
    lambda { get :new }.should raise_error
  end

  it "should raise an error on post create" do
    lambda { post :create, user: { name: @user.name, provider: @user.provider, uid: @user.uid }}.should raise_error
  end

  it "should raise an error on get show" do
    lambda { get :show, id: @user }.should raise_error
  end

  it "should raise an error on get edit" do
    lambda { get :edit, id: @user }.should raise_error
  end

  it "should raise an error on put update" do
    lambda { put :update, id: @user, user: { name: @user.name, provider: @user.provider, uid: @user.uid } }.should raise_error
  end

  it "should raise an error on destroy user" do
   lambda { delete :destroy, id: @user }.should raise_error
  end
end
