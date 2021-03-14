require "psych"
require "importers"

desc "Import yaml formatted entries."
task :import_yml, [:import_fn] => :environment do |t, args|
  yaml_str = File.read(args.import_fn)

  objs = Importers.import_from_yaml(yaml_str)

  objs.each { |o| o.save }
end
