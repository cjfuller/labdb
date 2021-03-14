class Search < ActiveRecord::Base
  belongs_to :user

  attr_accessor :loaded_result

  def expired?
    Time.now > self.expires.to_time
  end
end
