class AddAuthColumnsToUser < ActiveRecord::Migration
  def change
    add_column :users, :auth_read, :boolean
    add_column :users, :auth_write, :boolean
    add_column :users, :auth_admin, :boolean
  end
end
