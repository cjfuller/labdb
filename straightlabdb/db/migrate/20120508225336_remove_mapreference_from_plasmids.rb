class RemoveMapreferenceFromPlasmids < ActiveRecord::Migration
  def up
    remove_column :plasmids, :mapreference
      end

  def down
    add_column :plasmids, :mapreference, :string
  end
end
