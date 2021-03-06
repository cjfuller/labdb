require "exporters"
require "numbered"
require "described"
require "headings"
require "dna_sequence"
require "resource_helpers"

module CheckPlasmidSequence
  # Patches the Bacterium class to check a linked plasmid for a sequence
  # if there isn't one present.
  def sequence
    seq = super
    return seq if seq && !seq.empty?
    linked_plas = self.core_alt_link
    if linked_plas && !linked_plas.empty?
      linked_plas.each do |p|
        return p.sequence if p && p.sequence && !p.sequence.empty?
      end
    end
    seq
  end
end

class Bacterium < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include DNASequence
  include CheckPlasmidSequence
  include ResourceHelpers

  @headings = {
    strain_number: "#{obj_tag} Number",
    date_entered: "Date",
    entered_by: "Entered by",
    notebook: "Notebook",
    comments: "Description",
    plasmid_number: "#{Naming.name_for(Plasmid)} Number",
    species_bkg: "Species and background",
    genotype: "Genotype",
    sequence: "Sequence",
    strainalias: "Alias",
    intrinsic_resistance: "Intrinsic antibiotic resistances"
  }

  Fields = @headings.keys

  def linked_properties
    [:plasmid_number]
  end

  def get_linked(property_name)
    numbers = get_linked_number_fields(property_name)
    get_linked_plasmids(numbers) unless numbers.nil?
  end

  def exportable_fields
    Fields
  end

  def self.number_field_name
    :strain_number
  end

  def self.info_field_name
    :strainalias
  end

  def self.description_field_name
    :comments
  end

  def core_alt_field_name
    :plasmid_number
  end

  def self.owner_field_name
    :entered_by
  end

  def timestamp_field_name
    :date_entered
  end

  def core_alt_field
    numbers = get_linked_number_fields(core_alt_field_name) || []
    numbers.map { |n| Naming.name_for(Plasmid) + " " + n.to_s }
  end

  def core_alt_link
    links = get_linked(core_alt_field_name)
    (get_linked_number_fields(core_alt_field_name) || []).map { |n| links[n] }
  end

  def combined_resistances
    numbers = get_linked_number_fields(core_alt_field_name) || []
    plasmids = if numbers.nil? then [] else get_linked_plasmids(numbers).values end
    resistances = plasmids.flat_map do |p| 
      if p.nil? or p.antibiotic.nil?
        []
      else
        p.antibiotic
          .split(",")
          .map { |r| r.strip }
          .filter{ |r| !r.empty?}
          .map { |r| "#{r} (via #{Naming.name_for(Plasmid) + ' ' + p.number.to_s})"}
      end
    end
    resistances += (self.intrinsic_resistance || "")
     .split(",")
     .map { |r| r.strip }
     .filter { |r| !r.empty? }
    resistances = resistances.uniq
  end

  def core_info
    [
      {
        name: "Strain information",
        fields: fields([:species_bkg, :genotype]) + [
          field(:intrinsic_resistance).merge({
            precalculatedValue: combined_resistances.join(", "),
            name: "Antibiotic resistances",
          })
        ],
      },
      {
        name: "Description",
        preformatted: true,
        lookup: :comments,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN.render(comments || "")
                                                 .labdb_auto_link
                                                 .html_safe,
      },
    ]
  end

  def sequence_info
    {
      sequence: { lookup: :sequence },
      verified: nil,
    }
  end

  def supplemental_info
    fields [:entered_by, :date_entered, :notebook]
  end
end
