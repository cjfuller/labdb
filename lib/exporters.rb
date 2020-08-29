require "psych"
require "object_naming"

module Exportable
  FORMATS = {
    yml: :export_to_yaml,
    fasta: :export_to_fasta,
    json: :export_to_json,
  }.freeze

  def name_str
    Naming.name_for(self.class) + number_field.to_s
  end

  def get_export_params(format)
    exp_params = {}
    exp_params[:filename] = name_str + "." + format.to_s
    exp_params[:type] = "text/plain"
    exp_params
  end

  def export_to(format)
    self.send(FORMATS[format]) if FORMATS[format]
  end

  def export_to_yaml
    fields = exportable_fields
    output = {}
    fields.sort.each do |f|
      fe = f.to_s.encode("utf-8")
      output[fe] = self.send(f).to_s.encode("utf-8")
    end
    Psych.dump({ self.class.to_s.encode("utf-8") => output })
  end

  def export_to_json
    fields = exportable_fields
    output = {}
    fields.sort.each do |f|
      fe = f.to_s.encode("utf-8")
      output[fe] = self.send(f).to_s.encode("utf-8")
    end
    output["name"] = self.name_str
    output["database"] = Naming.name_for("database_full")
    JSON.pretty_generate(self.class.to_s.encode("utf-8") => output)
  end

  def export_to_fasta
    output = ""
    output << ">" << name_str << " " << info_field.to_s << "\n"
    output << wrap_string(self.sequence, 80)
    output
  end

  def has_sequence?
    self.respond_to?(:sequence) && !self.sequence.nil?
  end

  def wrap_string(str, length)
    str.scan(/.{#{length}}|.+/).join("\n")
  end
end
