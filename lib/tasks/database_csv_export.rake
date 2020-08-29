require "csv"

desc "Export all databases to csv format."
task :export_databases_to_csv, [:export_dir] => :environment do |t, args|
  DATABASES = [Plasmid, Oligo, Bacterium, Antibody, Yeaststrain]
  EXT = ".csv"

  DATABASES.each do |db|
    objs = db.all

    objs.sort do |a, b|
      a.number_field.to_i <=> b.number_field.to_i
    end

    headers = []

    unless objs.empty?
      headers = objs[0].exportable_fields

      headers.sort!

      headers.delete(objs[0].number_field_name)

      headers.unshift(objs[0].number_field_name)
    end

    output_fn = File.expand_path(db.to_s + EXT, args.export_dir)

    CSV.open(output_fn, "w") do |f_out|
      f_out << [headers, "type"].flatten

      objs.each do |obj|
        obj_row = headers.map { |e| obj.send(e) }

        obj_row << obj.class.to_s

        f_out << obj_row
      end
    end
  end
end
