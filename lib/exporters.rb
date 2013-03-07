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

require 'yaml'

module Exportable

	Formats = {yaml: :export_to_yaml, fasta: :export_to_fasta}

	def export_to(format)

		if Formats[format] then
			self.send(Formats[format])
		end

	end

	def export_to_yaml

		fields = exportable_fields

		output = {}

		fields.each do |f|

			output[f.to_s] = self.send(f).to_s

		end

		YAML.dump({self.class.to_s => output})

	end

	def export_to_fasta

	end

	def has_sequence?
		self.respond_to? :sequence
	end




end


