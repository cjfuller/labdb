class AddAttachmentPlasmidmapToPlasmids < ActiveRecord::Migration
  def self.up
    add_column :plasmids, :plasmidmap_file_name, :string
    add_column :plasmids, :plasmidmap_content_type, :string
    add_column :plasmids, :plasmidmap_file_size, :integer
    add_column :plasmids, :plasmidmap_updated_at, :datetime
  end

  def self.down
    remove_column :plasmids, :plasmidmap_file_name
    remove_column :plasmids, :plasmidmap_content_type
    remove_column :plasmids, :plasmidmap_file_size
    remove_column :plasmids, :plasmidmap_updated_at
  end
end
