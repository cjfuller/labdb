class AddSequenceToBacterium < ActiveRecord::Migration
  def change
    add_column :bacteria, :sequence, :text
  end
end
