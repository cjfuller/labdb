class Search < ActiveRecord::Base
  attr_accessible :expires, :result, :searchparams, :user_id

  belongs_to :user

  attr_accessor :loaded_result

  def expired?
  	Time.now > self.expires.to_time
  end

end
