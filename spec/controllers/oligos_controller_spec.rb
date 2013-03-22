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

require 'spec_helper'

describe OligosController do  

  extend CommonControllerSpecs

  search_tests model_class: Oligo, search_field: :organism, count_0_regexp: "/pipens/", count_1_regexp: "/tropicalis/", count_2_regexp: "/Xenopus/", non_regexp_exp: "Xenopus", ci_exp: "/LAEVIS/"
  
end
