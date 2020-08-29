class ChangeLineLocationsToText < ActiveRecord::Migration
  def up
    change_column :lines, :locations, :text
  end

  def down
    change_column :lines, :locations, :string
  end
end
