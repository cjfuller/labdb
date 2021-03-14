desc "Import databases from csv format."
task :import_csv, [:import_fn] => :environment do |t, args|
  csv_str = File.read(args.import_fn)

  objs = Importers.import_from_csv(csv_str)

  objs.each { |e| e.save }
end
