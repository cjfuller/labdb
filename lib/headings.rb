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

module Headings
  def self.included(base)
    base.class_eval do
      def self.get_heading(var_name)
        @headings[var_name]
      end

      def self.obj_tag
        Naming.name_for(self)
      end
    end
  end

  def get_heading(var_name)
    self.class.get_heading(var_name)
  end

  def obj_tag
    Naming.name_for(self.class)
  end
end
