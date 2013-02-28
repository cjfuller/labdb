class RenameOligoAlias < ActiveRecord::Migration
  def up
    rename_column :oligos, :alias, :oligoalias
  end

  def down
    rename column :oligos, :oligoalias, :alias
  end
end
