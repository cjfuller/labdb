class RemovePlasmidnumberFromPlasmid < ActiveRecord::Migration
  def up
    change_table :plasmids do |t|
      t.remove :plasmidnumber
    end
  end

  def down
    change_table :plasmids do |t|
      t.string :plasmidnumber
    end
  end
end
