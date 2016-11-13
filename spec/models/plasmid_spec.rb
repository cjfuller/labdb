require 'spec_helper'

describe Plasmid do

	fixtures :plasmids, :bacteria

  it "should link to its bacterial strain" do
		plasmids(:one).get_linked(:strainnumbers)["2"].should eq bacteria(:two)
	end

	it "should have the number field set to the plasmid number" do
		plasmids(:one).number_field.to_i.should eq 1
	end

	it "should list exportable fields" do
		plasmids(:one).exportable_fields.should_not be_empty
	end

	it "should name itself correctly" do
		plasmids(:one).name_str.should eq "#{Naming.name_for(Plasmid)}1"
	end

	it "should correctly set YAML export parameters" do
		plasmids(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Plasmid)}1.yml"
	end

	it "should correctly set FASTA export parameters" do
		plasmids(:one).get_export_params("fasta")[:filename].should eq "#{Naming.name_for(Plasmid)}1.fasta"
	end

	it "should correctly export to YAML" do
		yaml_str = <<YAML
Plasmid:
  antibiotic: carb,kan,gent
  concentration: '1.5'
  date_entered: '2012-05-02'
  description: CENP-I plasmid
  enteredby: Colin
  notebook: '1'
  plasmidalias: CENP-I
  plasmidnumber: '1'
  plasmidsize: '1'
  sequence: ATTTGAGAGAAA
  strainnumbers: '2'
  vector: pCS2
  verified: 'true'
YAML
		plasmids(:one).export_to_yaml.should eq yaml_str
	end


	it "should correctly export to FASTA" do
		fasta_str = ">#{Naming.name_for(Plasmid)}1 CENP-I\nATTTGAGAGAAA"
		plasmids(:one).export_to_fasta.should eq fasta_str
	end

	it "should correctly parse its antibiotics" do
		plasmids(:one).parse_antibiotics

		[:carb, :kan, :gent].each do |a|
			plasmids(:one).send(a).should eq "1"
		end

		[:chlor, :tet, :strep].each do |a|
			plasmids(:one).send(a).should eq "0"
		end
	end

	it "should correctly calculate its size" do
		plasmids(:one).calculate_size
		plasmids(:one).plasmidsize.should eq plasmids(:one).sequence.length
	end
end
