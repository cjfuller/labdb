class Oligo < ActiveRecord::Base
  attr_accessible :oligoalias, :date_entered, :entered_by, :notebook, :oligo_number, :organism, :purpose, :sequence, :vendor
end
