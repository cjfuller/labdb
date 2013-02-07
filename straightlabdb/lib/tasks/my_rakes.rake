require 'rexml/document'
require 'date'
require 'nokogiri'

def node_text(plas, name)

  ch = plas.xpath(name)[0].child

  return "" unless ch

  ch.text

end


desc "Import plasmids."
task :import_plasmids => :environment do


  doc = Nokogiri::XML(File.new("/home/cfuller/Desktop/plasmid_database_3.xml"))

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
          asbs << ", "
        end

        asbs << asbs_num

      end

    end

    conc = node_text(plas, "Concentration")

    date = node_text(plas, "Date_Frozen")

    if date.length > 0 then
      if /\d{1,2}\/\d{1,2}\/\d{4}/.match(date) then
        date=Date.strptime(date, '%m/%d/%Y')
      elsif /\d{1,2}-\d{1,2}-\d{4}/.match(date) then
        date=Date.strptime(date, '%m-%d-%Y')
      elsif /\d{4}-\d{1,2}-\d{1,2}/.match(date) then
        date=Date.strptime(date, '%Y-%m-%d')
      elsif /\d{4}\/\d{1,2}\/\d{1,2}/.match(date) then
        date=Date.strptime(date, '%Y/%m/%d')
      end
    end

    ent_by = node_text(plas, "Entered_by")

    nb = node_text(plas, "Notebook")

    verified = node_text(plas, "Sequence_verified")

    vector = node_text(plas, "Vector_with_vector_primers")

    seq = node_text(plas, "DNA_sequence")

    antibiotic = node_text(plas, "Antibiotic")

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

  #str = File.read("ASPlasmid.xml")
  #
  #doc = REXML::Document.new(str.gsub("\u0000",""))
  #
  #
  #doc.elements.each("FMPXMLRESULT/RESULTSET/ROW") do |r|
  #
  #  parts = []
  #
  #  r.elements.to_a("COL").each do |ele|
  #    data_eles = []
  #    ele.elements.to_a("DATA").each do |ele2|
  #      textpart = ele2.text
  #
  #      data_eles << textpart unless textpart.nil?
  #    end
  #    if data_eles.empty? then
  #      parts << ""
  #    else
  #      parts << data_eles.join(",")
  #    end
  #  end
  #
  #  antibiotic=parts[0].downcase
  #  comments=parts[1]
  #  asbs=parts[2]
  #  seq=parts[3]
  #  p_alias=parts[4]
  #  asp=parts[5]
  #  conc=parts[6]
  #  date=parts[7]
  #  if date.length > 0 then
  #    if /\d{1,2}\/\d{1,2}\/\d{4}/.match(date) then
  #      date=Date.strptime(date, '%m/%d/%Y')
  #    elsif /\d{1,2}-\d{1,2}-\d{4}/.match(date) then
  #      date=Date.strptime(date, '%m-%d-%Y')
  #    elsif /\d{4}-\d{1,2}-\d{1,2}/.match(date) then
  #      date=Date.strptime(date, '%Y-%m-%d')
  #    elsif /\d{4}\/\d{1,2}\/\d{1,2}/.match(date) then
  #      date=Date.strptime(date, '%Y/%m/%d')
  #    end
  #  end
  #  ent_by=parts[8]
  #  nb=parts[9]
  #  verified=parts[10]
  #  vector=parts[11]
  #
  #
  #  ab_string=""
  #
  #  abs = %w[carb kan chlor gent tet strep]
  #
  #  abs.each do |ab|
  #    if Regexp.new(ab).match(antibiotic)
  #      if ab_string.length > 0 then
  #        ab_string << ","
  #      end
  #      ab_string << ab
  #    end
  #  end
  #
  #  puts asp
  #
  #  options= {antibiotic: ab_string,
  #            concentration: conc,
  #            date_entered: date,
  #            description: comments,
  #            enteredby: ent_by,
  #            notebook: nb,
  #            plasmidalias: p_alias,
  #            plasmidnumber: asp,
  #            sequence: seq,
  #            plasmidsize: seq.length,
  #            strainnumbers: asbs,
  #            vector: vector,
  #            verified: verified}
  #
  #  #puts options
  #
  #  p = Plasmid.new(options)
  #
  #
  #  p.save
  #
  #
  #end



#end