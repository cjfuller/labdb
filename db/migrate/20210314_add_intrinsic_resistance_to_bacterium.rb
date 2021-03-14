class AddIntrinsicResistanceToBacterium < ActiveRecord::Migration[4.2]
  def change
    add_column :bacteria, :intrinsic_resistance, :string
  end
end