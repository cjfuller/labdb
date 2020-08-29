require "spec_helper"
require "auto_linked"

describe LinkableString do
  fixtures :oligos, :plasmids

  it "should identify and link an object reference" do
    ls = LinkableString.new("ASP1")
    ls.sub_labdb_links
    obj_id = Plasmid.where(Plasmid.number_field_name => 1).first.id
    ls.to_s.should match /href="\/plasmids\/#{obj_id}"/
  end

  it "should identify and link an object reference with optional N" do
    ls = LinkableString.new("ASON1")
    ls.sub_labdb_links
    obj_id = Oligo.where(Oligo.number_field_name => 1).first.id
    ls.to_s.should match /href="\/oligos\/#{obj_id}"/
  end

  it "should fail gracefully when encountering a nonexistent reference" do
    ls = LinkableString.new("ASON3444992")
    expect { ls.sub_labdb_links }.not_to raise_error
  end
end
