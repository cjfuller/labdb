class CreateYeaststrains < ActiveRecord::Migration
  def change
    create_table :yeaststrains do |t|
      t.string :strainalias
      t.string :antibiotic
      t.string :plasmidnumber
      t.integer :strain_number
      t.string :strain_bkg
      t.date :date_entered
      t.text :sequence
      t.string :entered_by
      t.text :comments
      t.text :genotype
      t.string :location
      t.string :species

      t.timestamps
    end
  end
end
