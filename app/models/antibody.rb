require "exporters"
require "numbered"
require "described"
require "headings"
require "resource_helpers"

class Antibody < ActiveRecord::Base
  include Exportable
  include Numbered
  include Described
  include Headings
  include ResourceHelpers

  @headings = {
    ab_number: "#{obj_tag} Number",
    date_entered: "Date entered",
    label: "Label",
    entered_by: "Entered by",
    alias: "Alias",
    comments: "Description",
    host: "Host",
    vendor: "Vendor",
    good_for_if: "Good for IF",
    good_for_western: "Good for westerns",
    fluorophore: "Fluorophores",
    box: "Box",
  }

  Fields = @headings.keys

  def get_linked(_propertyname)
    nil
  end

  def exportable_fields
    Fields
  end

  def self.number_field_name
    :ab_number
  end

  def self.info_field_name
    :alias
  end

  def self.description_field_name
    :comments
  end

  def self.timestamp_field_name
    :date_entered
  end

  def self.owner_field_name
    :entered_by
  end

  def core_info
    [
      {
        name: "Antibody information",
        fields: fields([:host, :fluorophore]),
      },
      {
        name: "Location information",
        fields: fields([:box, :label]),
      },
      {
        name: "Uses",
        fields: [
          field(:good_for_if, type: :boolean),
          field(:good_for_western, type: :boolean),
        ],
      },
      {
        name: "Description",
        preformatted: true,
        lookup: :comments,
        single: true,
        inlineValue: Labdb::Application::MARKDOWN
          .render(comments || "")
          .labdb_auto_link
          .html_safe,
      },
    ]
  end

  def sequence_info
    nil
  end

  def supplemental_info
    fields [:entered_by, :date_entered, :vendor]
  end
end
