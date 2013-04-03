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

describe YeaststrainsController do

  extend CommonControllerSpecs

  search_tests model_class: Yeaststrain, search_field: :strainalias, count_0_regexp: "/lys-/", count_1_regexp: "/ade-/", count_2_regexp: "/leu-/", non_regexp_exp: "leu", ci_exp: "/ADE/"

end