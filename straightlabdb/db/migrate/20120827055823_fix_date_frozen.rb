class FixDateFrozen < ActiveRecord::Migration
  def up
    rename_column :plasmids, :datefrozen, :date_entered
  end

  def down
    rename_column :plasmids, :date_entered, :datefrozen
  end
end
