class CreateSamples < ActiveRecord::Migration
  def change
    create_table :samples do |t|
      t.integer :sample_number
      t.string :sample_alias
      t.string :storage_type
      t.date :date_entered
      t.string :entered_by
      t.integer :notebook
      t.string :sample_type
      t.boolean :depleted
      t.text :description
      t.text :linked_items

      t.timestamps
    end
  end
end
