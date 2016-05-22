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


desc "Import yeast strains."
task :import_yeast => :environment do


  doc = Nokogiri::XML(File.new("db/db_imports/yeast_database.xml"))

  doc.remove_namespaces!

  yeasts = doc.xpath("//ROW")

  yeasts.each do |yeast|

    asys = node_text(yeast, "ASYS_Number")

    puts asys

    comments = node_text(yeast, "Features")

    date = parse_date(node_text(yeast, "Date_Frozen"))

    ent_by = node_text(yeast, "Entered_By")

    gen = node_text(yeast, "Genotype")

    loc = node_text(yeast, "Location")

    bkg = node_text(yeast, "Background")

    asp = node_text(yeast, "ASP_Number")

    strainalias = node_text(yeast, "Alias")

    antibiotic = node_text(yeast, "Antibiotic")

    seq = node_text(yeast, "DNA_Sequence")

    spec = node_text(yeast, "Species")

    options = {
      date_entered: date,
      entered_by: ent_by,
      strain_number: asys,
      comments: comments,
      genotype: gen,
      strain_bkg: bkg,
      plasmidnumber: asp,
      species: spec,
      sequence: seq,
      antibiotic: antibiotic,
      strainalias: strainalias,
      location: loc
    }

    y = Yeaststrain.new(options)

    y.save

  end

end
