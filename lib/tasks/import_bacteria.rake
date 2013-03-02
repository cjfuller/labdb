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
