require 'exporters'
require 'numbered'
require 'described'
require 'object_naming'
require 'headings'
require 'resource_helpers'

class Sample < ActiveRecord::Base
  include Exportable
  include LinkableModel
  include Numbered
  include Described
  include Headings
  include ResourceHelpers

  @headings = {
    date_entered: 'Date',
    depleted: 'Sample depleted?',
    description: 'Description',
    entered_by: 'Entered by',
    linked_items: 'Linked to',
    notebook: 'Notebook',
    sample_alias: 'Alias',
    sample_number: "#{obj_tag} number",
    sample_type: 'Sample type',
    storage_type: 'Storage location',
  }

  Fields = @headings.keys

  attr_accessible(*Fields)

  def exportable_fields
    Fields
  end

  def self.number_field_name
    :sample_number
  end

  def self.info_field_name
    :sample_alias
  end

  def self.description_field_name
    :description
  end

  def self.timestamp_field_name
    :date_entered
  end

  def self.owner_field_name
    :entered_by
  end

  def core_alt_field
    self.depleted ? ['Depleted'] : []
  end

  def self.core_alt_field_name
    :depleted
  end

  def self.core_alt_field_type
  end

  def sample_links
    linked_items
      .split(',')
      .map(&:strip)
      .map { |item| [item, item.item_links(items: true).first] }
      .map do |item|
        {
          link_text: item[0],
          link_desc: item[1].send(item[1].class.info_field_name)
        }
      end
  end

  def core_info
    [
      {
        name: 'Sample storage',
        fields: fields([:sample_type, :storage_type])
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
        name: 'Linked items',
        preformatted: true,
        lookup: :linked_items,
        inlineValue: (sample_links || [])
          .map { |lnk| lnk[:link_text] + ': ' + lnk[:link_desc] }
          .join('<br />')
          .labdb_auto_link
          .html_safe
      }
    ]
  end

  def sequence_info
    nil
  end

  def supplemental_info
    fields [:entered_by, :date_entered, :notebook]
  end
end
