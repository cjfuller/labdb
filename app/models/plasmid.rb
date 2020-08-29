# coding: utf-8
require 'json'

require 'exporters'
require 'numbered'
require 'described'
require 'headings'
require 'dna_sequence'
require 'plasmid_mapping'
require 'resource_helpers'

require 'facets/string/word_wrap'

class Plasmid < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include DNASequence
  include ResourceHelpers

  # TODO: be more deliberate about class instance var / class var / constant
  # choices
  @headings = {
    antibiotic: 'Antibiotic resistances',
    concentration: 'Concentration (μg/mL)',
    date: 'Date',
    description: 'Description',
    creator: 'Entered by',
    notebook: 'Notebook',
    alias: 'Alias',
    number: "#{obj_tag} Number",
    sequence: 'Sequence',
    strainnumbers: "#{Naming.name_for(Bacterium)} numbers",
    vector: 'Vector',
    verified: 'Sequence verified?',
  }

  Fields = @headings.keys

  attr_accessor :search_by_regex

  def calculate_size
    self.plasmidsize = self.sequence.gsub(/\W/, '').length
  end

  def linked_properties
    [:strainnumbers]
  end

  def get_linked(property_name)
    numbers = get_linked_number_fields(property_name)
    get_linked_bacterial_strains(numbers) unless numbers.nil?
  end

  def exportable_fields
    Fields
  end

  def self.number_field_name
    :number
  end

  def self.info_field_name
    :alias
  end

  def self.description_field_name
    :description
  end

  def self.core_alt_field_name
    :strainnumbers
  end

  def self.owner_field_name
    :creator
  end

  def self.timestamp_field_name
    :date
  end

  def core_alt_field
    numbers = get_linked_number_fields(core_alt_field_name) || []
    numbers.map { |n| Naming.name_for(Bacterium) + ' ' + n.to_s }
  end

  def plasmid_map
    @map || (@map = PlasmidMapping.map_for_plasmid(self).plasmidmap_json)
  end

  def core_info
    [
      {
        name: 'Vector information',
        fields: [
          field(:vector),
          field(:antibiotic)
        ]
      },
      {
        name: 'Description',
        preformatted: true,
        lookup: :description,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN.render(description || '')
                                                 .labdb_auto_link.html_safe
      }
    ]
  end

  def sequence_info
    {
      sequence: { lookup: :sequence },
      verified: { lookup: :verified },
    }
  end

  def supplemental_info
    [
      field(:creator),
      field(:date),
      field(:notebook),
      field(:concentration)
    ]
  end
end
