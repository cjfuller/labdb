require 'exporters'
require 'numbered'
require 'described'
require 'headings'
require 'dna_sequence'
require 'resource_helpers'

class Yeaststrain < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include DNASequence
  include ResourceHelpers

  @headings = {
    strain_number: "#{obj_tag} Number",
    date_entered: 'Date entered',
    entered_by: 'Entered by',
    notebook: 'Notebook',
    comments: 'Description',
    plasmidnumber: "#{Naming.name_for(Plasmid)} Number",
    strain_bkg: 'Strain background',
    genotype: 'Genotype',
    antibiotic: 'Antibiotics',
    location: 'Location in freezer',
    sequence: 'Sequence',
    species: 'Species',
    strainalias: 'Alias',
  }

  Fields = @headings.keys

  def linked_properties
    [:plasmidnumber]
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

  def self.core_alt_field_name
    :plasmidnumber
  end

  def self.timestamp_field_name
    :date_entered
  end

  def self.owner_field_name
    :entered_by
  end

  def core_alt_field
    numbers = get_linked_number_fields(core_alt_field_name) || []
    numbers.map { |n| Naming.name_for(Plasmid) + ' ' + n.to_s }
  end

  def core_alt_link
    links = get_linked(core_alt_field_name)
    get_linked_number_fields(core_alt_field_name || []).map { |n| links[n] }
  end

  def core_info
    [
      {
        name: 'Strain information',
        fields: fields([:species, :strain_bkg, :genotype, :antibiotic]),
      },
      {
        name: 'Description',
        preformatted: true,
        lookup: :comments,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN
          .render(comments || '')
          .labdb_auto_link
          .html_safe,
      }
    ]
  end

  def sequence_info
    {
      sequence: { lookup: :sequence },
      verified: nil,
    }
  end

  def supplemental_info
    fields [:entered_by, :date_entered, :notebook, :location]
  end
end
