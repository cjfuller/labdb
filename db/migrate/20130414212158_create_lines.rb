class CreateLines < ActiveRecord::Migration
  def change
    create_table :lines do |t|
      t.integer :line_number
      t.string :line_alias
      t.date :date_entered
      t.string :entered_by
      t.integer :notebook
      t.string :species
      t.text :parent_line
      t.text :sequence
      t.text :description
      t.string :plasmid_numbers
      t.string :selectable_markers
      t.string :locations
      t.text :current_stock_counts

      t.timestamps
    end
  end
end
