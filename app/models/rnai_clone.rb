require 'exporters'
require 'numbered'
require 'described'
require 'headings'
require 'resource_helpers'

class RnaiClone < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include ResourceHelpers

  @headings = {
    number: "#{obj_tag} number",
    alias: 'Alias',
    notebook: 'Notebook',
    description: 'Description',
    entered_by: 'Entered by',
    sequence_name: 'Sequence name',
    library: 'Library',
    host_strain: 'Host strain',
    plasmid_backbone: 'Plasmid backbone',
    antibiotic: 'Antibiotic',
    location: 'Location',
    sequenced: 'Sequenced?',
    created_at: 'Date entered',
  }

  Fields = @headings.keys
  attr_accessible(*Fields)

  def type
    'rnai_clone'
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

  def self.timestamp_field_name
    :created_at
  end

  def self.owner_field_name
    :entered_by
  end

  def core_info
    [
      {
        name: 'Clone info',
        fields: fields([
          :sequence_name, :library, :host_strain, :plasmid_backbone, :antibiotic
        ])
      },
      {
        name: 'Description',
        preformatted: true,
        lookup: :description,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN
          .render(description || '')
          .labdb_auto_link
          .html_safe
      },
      {
        name: 'Other info',
        fields: [field(:location), field(:sequenced, type: :boolean)]
      },
    ]
  end

  def sequence_info
    nil
  end

  def supplemental_info
    fields [:entered_by, :created_at, :notebook]
  end
end