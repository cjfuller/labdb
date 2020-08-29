require "psych"
require "csv"

module Importers
  def self.import_from_yaml(yaml_str)
    loaded = Psych.load_stream(yaml_str)

    objs = []

    loaded.each do |obj_info|
      classname = obj_info.keys[0]

      params = obj_info[classname]

      objs << Kernel.const_get(classname).new(params)
    end

    objs
  end

  def self.import_from_csv(csv_str)
    lines = CSV.parse(csv_str)

    headers = lines.shift.map(&:strip).map(&:to_sym)

    objs = []

    lines.each do |l|
      l.map! { |e| e.nil? ? "" : e }

      fields = l.map(&:strip)

      params = {}

      headers.each_with_index do |h, i|
        params[h] = fields[i]
      end

      type = params.delete(:type)

      objs << Kernel.const_get(type).new(params)
    end

    objs
  end
end
