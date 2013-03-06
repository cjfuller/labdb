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


desc "Import plasmids."
task :import_plasmids => :environment do


  doc = Nokogiri::XML(File.new("/db/db_imports/plasmid_database.xml"))

  doc.remove_namespaces!

  plasmids = doc.xpath("//ROW")

  plasmids.each do |plas|

    asp = node_text(plas, "ASP_Number")

    puts asp

    p_alias = node_text(plas, "Alias")

    comments = node_text(plas, "Comments")

    asbs_el = plas.xpath("ASBS_numbers")[0]

    asbs = ""

    asbs_el.children.each do |dat|

      next unless dat.child

      asbs_num = dat.child.text

      if asbs_num.length > 0 then

        if asbs.length > 0 then
          asbs << ","
        end

        asbs << asbs_num

      end

    end

    conc = node_text(plas, "Concentration")

    date = parse_date(node_text(plas, "Date_Frozen"))


    ent_by = node_text(plas, "Entered_by")

    nb = node_text(plas, "Notebook")

    verified = node_text(plas, "Sequence_verified")

    vector = node_text(plas, "Vector_with_vector_primers")

    seq = node_text(plas, "DNA_sequence")

    antibiotic = node_text(plas, "Antibiotic").downcase!

    puts "initial antibiotic string: #{antibiotic}"

    ab_string=""

    abs = %w[carb kan chlor gent tet strep]

    abs.each do |ab|
      if Regexp.new(ab).match(antibiotic)
        if ab_string.length > 0 then
          ab_string << ","
        end
        ab_string << ab
      end
    end

    if /ampi/.match(antibiotic)
      if ab_string.length > 0 then
        ab_string << ","
      end
      ab_string << "carb"
    end

    puts "matched antibiotics: #{ab_string}"

    options= {antibiotic: ab_string,
                concentration: conc,
                date_entered: date,
                description: comments,
                enteredby: ent_by,
                notebook: nb,
                plasmidalias: p_alias,
                plasmidnumber: asp,
                sequence: seq,
                plasmidsize: seq.length,
                strainnumbers: asbs,
                vector: vector,
                verified: verified}

    p = Plasmid.new(options)

    p.save

  end

end
