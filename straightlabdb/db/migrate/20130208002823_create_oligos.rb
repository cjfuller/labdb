class CreateOligos < ActiveRecord::Migration
  def change
    create_table :oligos do |t|
      t.integer :oligo_number
      t.string :alias
      t.date :date_entered
      t.string :entered_by
      t.integer :notebook
      t.string :vendor
      t.string :organism
      t.text :sequence
      t.text :purpose

      t.timestamps
    end
  end
end
