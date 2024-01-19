class ChangeDinoFields < ActiveRecord::Migration[6.0]
  def change
    add_column :dinos, :species, :string
    add_column :dinos, :genotype, :string
    add_column :dinos, :selectable_markers, :string
    remove_column :dinos, :strain_info, :string
  end
end
