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
require 'object_naming'

module Exportable

	Formats = {yml: :export_to_yaml, fasta: :export_to_fasta}

	def name_str
		Naming.name_for(self.class) + number_field.to_s
	end

	def get_export_params(format)

		exp_params = {}

		exp_params[:filename] = name_str + "." + format.to_s

		if format.to_s == "fasta" then
			exp_params[:type] = "text/plain"
		end

		exp_params
	end

	def export_to(format)

		if Formats[format] then
			self.send(Formats[format])
		end

	end

	def export_to_yaml

		fields = exportable_fields

		output = {}

		fields.each do |f|

			fe = f.to_s.encode('utf-8')

			output[fe] = self.send(f).to_s.encode('utf-8')

		end

		Psych.dump({self.class.to_s.encode('utf-8') => output})

	end


	def export_to_fasta

		output = ""

		output << ">" << name_str << " " << info_field.to_s << "\n"

		output << wrap_string(self.sequence, 80)

		output

	end

	def has_sequence?
		self.respond_to? :sequence and not self.sequence.nil?
	end

	def wrap_string(str, length)

		str.scan(/.{#{length}}|.+/).join("\n")

	end


end


