class User < ActiveRecord::Base
  attr_accessible :name, :provider, :uid
  
  def self.create_with_omniauth(auth)
    user = User.new
    user.provider = auth['provider']
    user.uid = auth['uid']
    user.name = auth['info']['name']
    user.save
    user
  end



end
