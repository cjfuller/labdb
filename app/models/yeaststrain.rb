
class Yeaststrain < ActiveRecord::Base
  attr_accessible :antibiotic, :comments, :date_entered, :entered_by, :genotype, :location, :plasmidnumber, :sequence, :species, :strain_bkg, :strain_number, :strainalias, :notebook

  include LinkableModel

  def get_linked(property_name)
  	return nil unless property_name == :plasmidnumber
  	return nil if self.plasmidnumber.nil?
  	numbers = self.plasmidnumber.split(",").map! { |e| e.strip }
  	get_linked_plasmids(numbers)
  end

end
