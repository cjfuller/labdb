class CreateSearches < ActiveRecord::Migration
  def change
    create_table :searches do |t|
      t.text :searchparams
      t.integer :user_id
      t.date :expires
      t.text :result

      t.timestamps
    end
  end
end
