class CreateDinos < ActiveRecord::Migration[6.0]
  def change
    create_table :dinos do |t|
      t.integer :number
      t.string :alias
      t.string :strain_info
      t.string :description
      t.string :entered_by
      t.string :notebook

      t.timestamps
    end
  end
end
