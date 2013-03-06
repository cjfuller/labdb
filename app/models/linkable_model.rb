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

module LinkableModel

  def get_linked_items(klass, field, field_values)
    field_values.delete_if { |e| e == "none" }
    linked = {}
    field_values.each do |n|
      linked[n] = klass.where(field => n)[0]
    end
    linked
  end

  def get_linked_plasmids(plasmid_numbers)
    get_linked_items(Plasmid, :plasmidnumber, plasmid_numbers)
  end

  def get_linked_bacterial_strains(strain_numbers)
    get_linked_items(Bacterium, :strain_number, strain_numbers)
  end


end

