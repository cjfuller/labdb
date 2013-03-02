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
