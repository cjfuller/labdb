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

DATABASES = [Plasmid, Oligo, Bacteria, Antibody, Yeast]

desc "Export all databases to yaml format."
task :export_databases_to_yaml, [:export_dir] => :environment do |t, args|

	DATABASES.each do |db|

		objs = db.all

		objs.sort do |a,b|
			a.number_field.to_i <=> b.number_field.to_i
		end

		output_fn = File.expand_path(db.to_s, args.export_dir)

		File.open(output_fn, 'w') do |f_out|

			objs.each do |obj|

				f_out.puts obj.export_to_yaml

			end

		end

	end
	
end
