#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

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
