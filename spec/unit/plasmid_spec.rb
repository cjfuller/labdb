require 'spec_helper'

describe Plasmid do

	fixtures :plasmids, :bacteria

  it "should link to its bacterial strain" do

		plasmids(:one).get_linked(:strainnumbers)["2"].should eq bacteria(:two)

	end

	it "should have the number field set to the strain number" do

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

		yaml_str = "---\nPlasmid:\n  antibiotic: carb,kan,gent\n  concentration: '1.5'\n  date_entered: '2012-05-02'\n  description: CENP-I plasmid\n  enteredby: Colin\n  notebook: '1'\n  plasmidalias: CENP-I\n  plasmidnumber: '1'\n  plasmidsize: '1'\n  sequence: ATTTGAGAGAAA\n  strainnumbers: '2'\n  vector: pCS2\n  verified: 'true'\n"

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
