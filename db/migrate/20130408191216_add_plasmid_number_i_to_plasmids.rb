class AddPlasmidNumberIToPlasmids < ActiveRecord::Migration
  def change
    add_column :plasmids, :plasmidnumber_i, :integer
  end
end
