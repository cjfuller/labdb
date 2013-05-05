class AddStockPersonToLine < ActiveRecord::Migration
  def change
    add_column :lines, :stock_person, :text
  end
end
