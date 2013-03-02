class AddNotebookToYeaststrains < ActiveRecord::Migration
  def change
    add_column :yeaststrains, :notebook, :integer
  end
end
