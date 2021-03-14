require "spec_helper"

describe Search do
  it "should calculate when it's expired" do
    s = Search.new(expires: (Time.now - 10).to_date, result: "", searchparams: "", user_id: 1)

    s.should be_expired
  end

  it "should know if it's not expired" do
    s = Search.new(expires: (Time.now + 10 ** 6).to_date, result: "", searchparams: "", user_id: 1)

    s.should_not be_expired
  end
end
