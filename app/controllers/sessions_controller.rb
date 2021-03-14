require "number_assignment"

class SessionsController < ApplicationController
  skip_before_action :require_authorization, :only => [:create, :failure, :destroy]

  def new
  end

  def create
    reset_session
    auth_hash = request.env["omniauth.auth"]
    id = auth_hash["info"]["email"]
    user = User.find_by_email(id)

    if user
      session[:user_id] = user.email
      redirect_to "/", notice: "You have successfully logged in."
    else
      session[:user_id] = nil
      failure
    end
  end

  def failure
    redirect_to "/", notice: "Authorization failed."
  end

  def destroy
    session[:user_id] = nil
    redirect_to "/", notice: "You have successfully logged out."
  end
end
