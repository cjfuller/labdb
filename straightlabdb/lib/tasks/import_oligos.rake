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
