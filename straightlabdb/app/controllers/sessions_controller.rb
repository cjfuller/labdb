require 'number_assignment'

class SessionsController < ApplicationController

  skip_before_filter :require_authorization, :only => [:create, :failure, :destroy]

  def new
  end

  def create

    reset_session

    auth_hash = request.env['omniauth.auth']
    provider = auth_hash['provider']
    id = auth_hash['uid']
    user = User.find_by_uid(id)

    unless user then
      user = User.create_with_omniauth(auth_hash)
      user.save
    end

    session[:user_id] = user.uid

    redirect_to '/', notice: "You have successfully logged in."

  end

  def failure

    redirect_to '/', notice: "Authorization failed."

  end

  def destroy

    session[:user_id] = nil

    redirect_to '/', notice: "You have successfully logged out."

  end

end
