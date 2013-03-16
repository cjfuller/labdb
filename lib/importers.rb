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

require 'psych'
require 'csv'

module Importers

	def self.import_from_yaml(yaml_str)

		loaded = Psych.load_stream(yaml_str)

		objs = []

		loaded.each do |obj_info|

			classname = obj_info.keys[0]

			params = obj_info[classname]

			objs << Kernel.const_get(classname).new(params)

		end

		objs

	end


	def self.import_from_csv(csv_str)

		lines = CSV.parse(csv_str)

		headers = lines.shift.map(&:strip).map(&:to_sym)

		objs = []

		lines.each do |l|

			fields = l.map(&:strip)

			params = {}

			headers.each_with_index do |h, i|

				params[h] = fields[i]

			end

			type = params.delete(:type)

			objs << Kernel.const_get(type).new(params)

		end

		objs

	end

end

