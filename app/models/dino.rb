require "json"

require "exporters"
require "numbered"
require "described"
require "headings"
require "resource_helpers"

class Dino < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include ResourceHelpers

  @headings = {
    number: "#{obj_tag} number",
    alias: "Alias",
    notebook: "Notebook",
    description: "Description",
    entered_by: "Entered by",
    genotype: "Genotype",
    species: "Species",
    selectable_markers: "Selectable markers",
    created_at: "Date created"
  }

  Fields = @headings.keys

  def type
    "dino"
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
        name: "Strain information",
        fields: [
          field(:species),
          field(:genotype),
          field(:selectable_markers)
        ]
      },
      {
        name: "Description",
        preformatted: true,
        lookup: :description,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN.render(description || "").labdb_auto_link.html_safe
      }
    ]
  end

  def sequence_info
    nil
  end

  def supplemental_info
    [
      field(:entered_by),
      field(:created_at),
      field(:notebook)
    ]
  end
end
