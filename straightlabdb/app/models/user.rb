class User < ActiveRecord::Base

  attr_protected :name, :provider, :uid, :email
  
  def self.create_with_omniauth(auth)
    user = User.new
    user.provider = auth['provider']
    user.uid = auth['uid']
    user.email = auth['info']['email']
    user.name = auth['info']['name']
    user.save
    user
  end



end
