require 'rexml/document'
require 'date'

desc "Import plasmids."
task :import_plasmids => :environment do

  str = File.read("ASPlasmid.xml")

  doc = REXML::Document.new(str.gsub("\u0000",""))


  doc.elements.each("FMPXMLRESULT/RESULTSET/ROW") do |r|

    parts = []

    r.elements.to_a("COL").each do |ele|
      data_eles = []
      ele.elements.to_a("DATA").each do |ele2|
        textpart = ele2.text

        data_eles << textpart unless textpart.nil?
      end
      if data_eles.empty? then
        parts << ""
      else
        parts << data_eles.join(",")
      end
    end

    antibiotic=parts[0].downcase
    comments=parts[1]
    asbs=parts[2]
    seq=parts[3]
    p_alias=parts[4]
    asp=parts[5]
    conc=parts[6]
    date=parts[7]
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
    ent_by=parts[8]
    nb=parts[9]
    verified=parts[10]
    vector=parts[11]


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

    puts asp

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

    #puts options

    p = Plasmid.new(options)


    p.save


  end



end