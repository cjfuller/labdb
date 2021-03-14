desc "Change plasmid numbers to integers."
task :convert_plasmid_numbers => :environment do
  Plasmid.all.each do |p|
    p.plasmidnumber_i = p.plasmidnumber.to_i
    p.save
  end
end
