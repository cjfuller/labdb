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

require 'rexml/document'
require 'date'
require 'nokogiri'

require_relative 'import_helpers'


desc "Import oligos."
task :import_oligos => :environment do


  doc = Nokogiri::XML(File.new("db/db_imports/oligo_database.xml"))

  doc.remove_namespaces!

  oligos = doc.xpath("//ROW")

  oligos.each do |oligo|

    aso = node_text(oligo, "Oligo_Number")

    puts aso

    o_alias = node_text(oligo, "Alias")

    comments = node_text(oligo, "Purpose")

    date = parse_date(node_text(oligo, "Date"))

    ent_by = node_text(oligo, "Entered_by")

    nb = node_text(oligo, "Notebook")

    vendor = node_text(oligo, "Vendor")

    org = node_text(oligo, "Organism")

    seq = node_text(oligo, "Sequence")

    options = {
      oligoalias: o_alias, 
      date_entered: date,
      entered_by: ent_by,
      notebook: nb,
      vendor: vendor,
      oligo_number: aso,
      organism: org,
      purpose: comments,
      sequence: seq
    }

    o = Oligo.new(options)

    o.save

  end

end
