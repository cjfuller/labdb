class CreatePlasmids < ActiveRecord::Migration
  def change
    create_table :plasmids do |t|
      t.string :plasmidnumber
      t.date :datefrozen
      t.string :enteredby
      t.integer :notebook
      t.boolean :verified
      t.string :plasmidalias
      t.string :antibiotic
      t.integer :plasmidsize
      t.float :concentration
      t.string :strainnumbers
      t.text :description
      t.text :sequence
      t.string :vector
      t.string :mapreference

      t.timestamps
    end
  end
end
