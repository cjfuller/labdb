require "spec_helper"

describe Bacterium do
  fixtures :bacteria, :plasmids

  it "should link to its plasmid" do
    bacteria(:one).get_linked(:plasmid_number)["2"].should eq plasmids(:two)
  end

  it "should have the number field set to the strain number" do
    bacteria(:one).number_field.to_i.should eq 1
  end

  it "should list exportable fields" do
    bacteria(:one).exportable_fields.should_not be_empty
  end

  it "should name itself correctly" do
    bacteria(:one).name_str.should eq "#{Naming.name_for(Bacterium)}1"
  end

  it "should correctly set YAML export parameters" do
    bacteria(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Bacterium)}1.yml"
  end

  it "should correctly export to YAML" do
    yaml_str = <<~YAML
      ---
      Bacterium:
        comments: strain 1
        date_entered: '2013-03-02'
        entered_by: Colin
        genotype: ''
        notebook: '1'
        plasmid_number: '2'
        sequence: GATTTAGAGCC
        species_bkg: E. coli DH5a
        strain_number: '1'
        strainalias: alias 1
    YAML
    bacteria(:one).export_to_yaml.should eq yaml_str
  end
end
