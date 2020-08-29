desc "Create an initial admin user."
namespace :user do
  task :create, [:email] => :environment do |t, args|
    u = User.new
    u.email = args.email
    u.name = args.email
    u.auth_read = true
    u.auth_write = true
    u.auth_admin = true
    u.save
  end
end
