class StaticController < ApplicationController
  skip_before_action :require_authorization, :only => [:index]

  def index
    @logged_in = (not(session[:user_id].nil?))
    @login_page = true
    @labdb_name = Naming.name_for("database_full")
    @search_results = JSON.generate([])
    @content_json = JSON.generate([])
  end
end
