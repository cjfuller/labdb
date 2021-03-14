require "spec_helper"

describe Yeaststrain do
  fixtures :yeaststrains, :plasmids

  it "should link to its plasmid" do
    yeaststrains(:one).get_linked(:plasmidnumber)["2"].should eq plasmids(:two)
  end

  it "should have the number field set to the strain number" do
    yeaststrains(:one).number_field.to_i.should eq 1
  end

  it "should list exportable fields" do
    yeaststrains(:one).exportable_fields.should_not be_empty
  end

  it "should name itself correctly" do
    yeaststrains(:one).name_str.should eq "#{Naming.name_for(Yeaststrain)}1"
  end

  it "should correctly set YAML export parameters" do
    yeaststrains(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Yeaststrain)}1.yml"
  end

  it "should correctly set FASTA export parameters" do
    yeaststrains(:one).get_export_params("fasta")[:filename].should eq "#{Naming.name_for(Yeaststrain)}1.fasta"
  end

  it "should correctly export to YAML" do
    yaml_str = <<~YAML
      ---
      Yeaststrain:
        antibiotic: ''
        comments: ''
        date_entered: '2013-03-02'
        entered_by: Colin
        genotype: leu1- ura4-
        location: \"-80 S2R2\"
        notebook: ''
        plasmidnumber: '2'
        sequence: AATAAGAGAGC
        species: pombe
        strain_bkg: 972 h-
        strain_number: '1'
        strainalias: leu-/ura- h-
    YAML

    yeaststrains(:one).export_to_yaml.should eq yaml_str
  end

  it "should correctly export to FASTA" do
    fasta_str = ">#{Naming.name_for(Yeaststrain)}1 leu-/ura- h-\nAATAAGAGAGC"

    yeaststrains(:one).export_to_fasta.should eq fasta_str
  end
end
