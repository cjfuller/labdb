require "spec_helper"

describe Sample do
  fixtures :samples, :plasmids, :bacteria

  it "should link to its plasmid" do
    samples(:one).sample_links[0][:link_desc].should eq plasmids(:one).alias
  end

  it "should link to its strain" do
    samples(:one).sample_links[1][:link_desc].should eq bacteria(:two).strainalias
  end

  it "should link to another sample" do
    samples(:one).sample_links[2][:link_desc].should eq samples(:two).sample_alias
  end

  it "should have the number field set to the sample number" do
    samples(:one).number_field.to_i.should eq 1
  end

  it "should list exportable fields" do
    samples(:one).exportable_fields.should_not be_empty
  end

  it "should name itself correctly" do
    samples(:one).name_str.should eq "#{Naming.name_for(Sample)}1"
  end

  it "should correctly set YAML export parameters" do
    samples(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Sample)}1.yml"
  end

  it "should correctly export to YAML" do
    yaml_str = "---\nSample:\n  date_entered: '2013-04-15'\n  depleted: 'false'\n  description: Human CENP-A/H4 tetramer expressed using the polycistronic expression\n    system, 1.1 mg/mL\n  entered_by: Colin Fuller\n  linked_items: ASP1, ASBS2, SLS2\n  notebook: '1'\n  sample_alias: Human CENP-A/H4 tetramer\n  sample_number: '1'\n  sample_type: protein\n  storage_type: long-term\n"

    samples(:one).export_to_yaml.should eq yaml_str
  end
end
