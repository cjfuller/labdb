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

module PlasmidsHelper

  def truncate_string(str, max_length=20)

    str unless str.length > max_length

    str[0...max_length]

  end

  def readonly_for(property_list)
    property_list.inject({}) { |a, e| a[e]= readonly?; a }
  end

  
  def show_map?
    @show_map
  end


end
