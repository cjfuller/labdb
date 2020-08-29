require "exporters"
require "numbered"
require "described"
require "headings"
require "resource_helpers"

class SeqLib < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include ResourceHelpers

  @headings = {
    genome: "Genome",
    method: "Method",
    entered_by: "Entered by",
    project: "Project",
    storage_location: "Storage location",
    concentration: "Concentration",
    size_distribution: "Size distribution",
    index_id: "Index ID",
    index_seq: "Index sequence",
    description: "Description",
    linked_items: "Linked items",
    alias: "Alias",
    notebook: "Notebook",
    number: "#{obj_tag} number",
    created_at: "Date entered",
  }

  Fields = @headings.keys

  def type
    "seq_lib"
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
        name: "Core info",
        fields: fields([:genome, :method, :project, :storage_location, :concentration, :size_distribution, :index_id, :index_seq]),
      },
      {
        name: "Description",
        preformatted: true,
        lookup: :description,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN
          .render(description || "")
          .labdb_auto_link
          .html_safe,
      },
      {
        name: "Linked items",
        preformatted: true,
        lookup: :linked_items,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN
          .render(linked_items || "")
          .labdb_auto_link
          .html_safe,
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
