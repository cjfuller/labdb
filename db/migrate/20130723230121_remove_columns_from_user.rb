class RemoveColumnsFromUser < ActiveRecord::Migration
  def up
    remove_column :users, :uid
    remove_column :users, :provider
  end

  def down
    add_column :users, :provider, :string
    add_column :users, :uid, :string
  end
end
