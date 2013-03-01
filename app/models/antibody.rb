class Antibody < ActiveRecord::Base
  attr_accessible :ab_number, :alias, :box, :comments, :entered_by, :fluorophore, :good_for_if, :good_for_western, :host, :label, :vendor, :date_entered
end
