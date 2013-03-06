
class Bacterium < ActiveRecord::Base
  attr_accessible :comments, :date_entered, :entered_by, :genotype, :notebook, :plasmid_number, :species_bkg, :strain_number

  include LinkableModel

  def get_linked(property_name)
  	return nil unless property_name == :plasmid_number
  	 return nil if self.plasmid_number.nil?
  	numbers = self.plasmid_number.split(",").map! { |e| e.strip }
  	get_linked_plasmids(numbers)
  end

end
