require 'rexml/document'
require 'date'
require 'nokogiri'

require_relative 'import_helpers'


def good_for_opts(ab)

  if_exp = /Immunofluorescence/

  west_exp = /Western blots/

  text = node_text(ab, "Good_for")

  {if: if_exp.match(text) ? true : false, western: west_exp.match(text) ? true : false }

end

desc "Import antibodies."
task :import_antibodies => :environment do


  doc = Nokogiri::XML(File.new("db/db_imports/antibody_database.xml"))

  doc.remove_namespaces!

  antibodies = doc.xpath("//ROW")

  antibodies.each do |ab|

    aso = node_text(ab, "ASAB_Number")

    puts aso

    ab_alias = node_text(ab, "Alias")

    host = node_text(ab, "Host")

    label = node_text(ab, "Label")

    box = node_text(ab, "Box")

    comments = node_text(ab, "comments")

    ent_by = node_text(ab, "Entered_By")

    vendor = node_text(ab, "Vendor")

    fl = node_text(ab, "Fluorophore")

    good_for_if = good_for_opts(ab)[:if]

    good_for_western = good_for_opts(ab)[:western]


    options = {
        alias: ab_alias,
        date_entered: "",
        entered_by: ent_by,
        vendor: vendor,
        ab_number: aso,
        host: host,
        comments: comments,
        fluorophore: fl,
        good_for_if: good_for_if,
        good_for_western: good_for_western,
        box: box,
        label: label
    }

    o = Antibody.new(options)

    o.save

  end

end
