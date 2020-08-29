class UsersController < ApplicationController
  before_action :require_admin
  before_action :safeguard_self_modification, except: [:index]

  include StandardActions

  def model_class
    User
  end

  def obj_tag
    "User"
  end

  def define_table_view_vars
    @table_columns = { sort: :id, others: [:name, :email] }
    @controller = self.class
  end

  def safeguard_self_modification
    return unless params[:id]
    @user = User.find(params[:id])
    if @user == curr_user_obj
      redirect_to users_path and return
    end
  end
end
