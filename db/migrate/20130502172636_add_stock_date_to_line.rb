class AddStockDateToLine < ActiveRecord::Migration
  def change
    add_column :lines, :stock_date, :text
  end
end
