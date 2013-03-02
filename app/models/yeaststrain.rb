class Yeaststrain < ActiveRecord::Base
  attr_accessible :antibiotic, :comments, :date_entered, :entered_by, :genotype, :location, :plasmidnumber, :sequence, :species, :strain_bkg, :strain_number, :strainalias, :notebook
end
