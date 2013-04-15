class AddGenotypeToLine < ActiveRecord::Migration
  def change
    add_column :lines, :genotype, :text
  end
end
