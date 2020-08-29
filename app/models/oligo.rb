require 'json'

require 'exporters'
require 'numbered'
require 'described'
require 'headings'
require 'dna_sequence'
require 'resource_helpers'

class Oligo < ActiveRecord::Base
  include Exportable
  include Numbered
  include Described
  include Headings
  include DNASequence
  include ResourceHelpers

  @headings = {
    oligo_number: "#{obj_tag} Number",
    date_entered: 'Date',
    entered_by: 'Entered by',
    notebook: 'Notebook',
    oligoalias: 'Alias',
    purpose: 'Description',
    sequence: 'Sequence',
    organism: 'Organism',
    vendor: 'Vendor',
  }

  Fields = @headings.keys

  def get_linked(_propertyname)
    nil
  end

  def exportable_fields
    Fields
  end

  def self.number_field_name
    :oligo_number
  end

  def self.info_field_name
    :oligoalias
  end

  def self.description_field_name
    :purpose
  end

  def self.owner_field_name
    :entered_by
  end

  def self.timestamp_field_name
    :date_entered
  end

  def core_info
    [
      {
        name: 'Description',
        preformatted: true,
        lookup: :purpose,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN.render(purpose || '')
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
    fields [:entered_by, :date_entered, :notebook, :organism, :vendor]
  end
end
