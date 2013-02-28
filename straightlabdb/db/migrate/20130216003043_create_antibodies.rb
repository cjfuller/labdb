class CreateAntibodies < ActiveRecord::Migration
  def change
    create_table :antibodies do |t|
      t.integer :ab_number
      t.string :host
      t.string :label
      t.string :box
      t.string :alias
      t.string :fluorophore
      t.string :entered_by
      t.boolean :good_for_if
      t.boolean :good_for_western
      t.text :comments
      t.string :vendor

      t.timestamps
    end
  end
end
