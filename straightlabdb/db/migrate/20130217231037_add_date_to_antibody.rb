class AddDateToAntibody < ActiveRecord::Migration
  def change
    add_column :antibodies, :date_entered, :date
  end
end
