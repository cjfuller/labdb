require "yaml"

module Naming
  NAMES_FN = Rails.root.join("config/db_names.yml").to_s

  NAMES = YAML.load(File.read(NAMES_FN))

  NAMES_LOOKUP = NAMES.invert

  def self.name_for(objtype)
    NAMES[objtype.to_s]
  end

  def self.named_class_for(objname)
    NAMES_LOOKUP[objname]
  end
end
