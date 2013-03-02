class CreateBacteria < ActiveRecord::Migration
  def change
    create_table :bacteria do |t|
      t.integer :strain_number
      t.string :species_bkg
      t.date :date_entered
      t.string :entered_by
      t.integer :notebook
      t.text :genotype
      t.text :comments
      t.string :plasmid_number

      t.timestamps
    end
  end
end
