describe User do
  before :each do
    @u = User.new
    @perms = [:auth_read, :auth_write, :auth_admin]
  end

  it "should have no permissions by default" do
    @perms.each do |perm|
      @u.send(perm).should be_falsey
    end
  end

  it "should toggle permissions correctly" do
    @perms.each do |perm|
      @u.toggle_auth_field!(perm)
      @u.send(perm).should be true
    end
    @perms.each do |perm|
      @u.toggle_auth_field!(perm)
      @u.send(perm).should be false
    end
  end
end
