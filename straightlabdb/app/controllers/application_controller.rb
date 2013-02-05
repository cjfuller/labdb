require 'psych'

class ApplicationController < ActionController::Base

  protect_from_forgery


  AUTH_FN = Rails.root.join('config/authusers.yml')
  ALLOWED_LABEL = "allowedusers"
  NAME_TAG = "name"
  EMAIL_TAG = "email"

  def denied
    render :text => "Access denied."
  end

  before_filter :require_authorization

  def require_authorization

    unless authorized? then

      redirect_to "/", notice: "Access denied."

    end

  end

  def load_auth

    @user_data = nil

    File.open(AUTH_FN, 'r') do |f|
      @user_data = Psych.load(f.read)
    end

    @users_loaded = true

  end


  def authorized?

    curr_uid = session[:user_id]

    return false if curr_uid.nil?

    curr_user = User.find_by_uid(curr_uid)

    load_auth unless @users_loaded

    allowed_users = @user_data[ALLOWED_LABEL]

    puts curr_user.inspect

    allowed_users.each do |u|

      puts u


      if curr_user.email == u[EMAIL_TAG] and curr_user.name == u[NAME_TAG] then
        return true
      end

    end

    false

  end


end
