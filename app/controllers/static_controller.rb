class StaticController < ApplicationController

  skip_before_filter :require_authorization, :only => [:index]

  def index
    @logged_in = (not (session[:user_id].nil?))
  end

end
