class ChangeNotebookFieldsToFloat < ActiveRecord::Migration
  def up
    change_column :plasmids, :notebook, :string
    change_column :oligos, :notebook, :string
    change_column :bacteria, :notebook, :string
    change_column :yeaststrains, :notebook, :string
    change_column :lines, :notebook, :string
    change_column :samples, :notebook, :string
  end

  def down
    change_column :plasmids, :notebook, :integer
    change_column :oligos, :notebook, :integer
    change_column :bacteria, :notebook, :integer
    change_column :yeaststrains, :notebook, :integer
    change_column :lines, :notebook, :integer
    change_column :samples, :notebook, :integer
  end
end
