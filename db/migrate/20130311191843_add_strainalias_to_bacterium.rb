class AddStrainaliasToBacterium < ActiveRecord::Migration
  def change
    add_column :bacteria, :strainalias, :string
  end
end
