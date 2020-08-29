class RenamePlasmidNumberIInPlasmid < ActiveRecord::Migration
  def change
    rename_column :plasmids, :plasmidnumber_i, :plasmidnumber
  end
end
