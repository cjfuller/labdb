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


desc "Import bacterial strains."
task :import_bacteria => :environment do


  doc = Nokogiri::XML(File.new("db/db_imports/bacteria_database.xml"))

  doc.remove_namespaces!

  bacts = doc.xpath("//ROW")

  bacts.each do |bact|

    asbs = node_text(bact, "ASBS_Number")

    puts asbs

    comments = node_text(bact, "Comments")

    date = parse_date(node_text(bact, "Date_Frozen"))

    ent_by = node_text(bact, "Entered_by")

    nb = node_text(bact, "Notebook")

    gen = node_text(bact, "Genotype")

    bkg = node_text(bact, "Species_Background")

    asp = node_text(bact, "ASP_Number")

    options = {
      date_entered: date,
      entered_by: ent_by,
      notebook: nb,
      strain_number: asbs,
      comments: comments,
      genotype: gen,
      species_bkg: bkg,
      plasmid_number: asp
    }

    b = Bacterium.new(options)

    b.save

  end

end
