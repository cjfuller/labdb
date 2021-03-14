desc "Export all databases to yaml format."
task :export_databases_to_yaml, [:export_dir] => :environment do |t, args|
  DATABASES = [Plasmid, Oligo, Bacterium, Antibody, Yeaststrain]
  EXT = ".yml"

  DATABASES.each do |db|
    objs = db.all

    objs.sort do |a, b|
      a.number_field.to_i <=> b.number_field.to_i
    end

    output_fn = File.expand_path(db.to_s + EXT, args.export_dir)

    File.open(output_fn, "w") do |f_out|
      objs.each do |obj|
        f_out.puts obj.export_to_yaml
      end
    end
  end
end
