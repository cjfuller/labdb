class Bacterium < ActiveRecord::Base
  attr_accessible :comments, :date_entered, :entered_by, :genotype, :notebook, :plasmid_number, :species_bkg, :strain_number
end
