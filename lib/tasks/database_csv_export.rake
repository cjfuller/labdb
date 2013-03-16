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

require 'csv'

desc "Export all databases to csv format."
task :export_databases_to_csv, [:export_dir] => :environment do |t, args|

	DATABASES = [Plasmid, Oligo, Bacterium, Antibody, Yeaststrain]
	EXT = ".csv"

	DATABASES.each do |db|

		objs = db.all

		objs.sort do |a,b|
			a.number_field.to_i <=> b.number_field.to_i
		end

		headers = []

		unless objs.empty? then

			headers = objs[0].exportable_fields

			headers.sort!

			headers.delete(objs[0].number_field_name)

			headers.unshift(objs[0].number_field_name)

		end

		output_fn = File.expand_path(db.to_s + EXT, args.export_dir)

		CSV.open(output_fn, 'w') do |f_out|

			f_out << [headers, "type"].flatten

			objs.each do |obj|

				obj_row = headers.map { |e| obj.send(e) }

				obj_row << obj.class.to_s

				f_out << obj_row

			end

		end

	end
	
end
