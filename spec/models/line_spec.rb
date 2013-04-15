require 'spec_helper'

describe Line do

	fixtures :lines, :plasmids

  it "should link to its plasmid" do

		lines(:one).get_linked(:plasmid_numbers)["1"].should eq plasmids(:one)

	end

	it "should have the number field set to the sample number" do

		lines(:one).number_field.to_i.should eq 1

	end

	it "should list exportable fields" do

		lines(:one).exportable_fields.should_not be_empty

	end

	it "should name itself correctly" do

		lines(:one).name_str.should eq "#{Naming.name_for(Line)}1"

	end

	it "should correctly set YAML export parameters" do

		lines(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Line)}1.yml"

	end

	it "should correctly export to YAML" do

		yaml_str = "---\nLine:\n  current_stock_counts: '1,3'\n  date_entered: '2013-04-15'\n  description: N-terminal GFP-CENP-A HeLa constitutive expressing\n  entered_by: Colin Fuller\n  line_alias: GFP-CENP-A HeLa\n  line_number: '1'\n  locations: lN2R1S1,lN2R1S2\n  notebook: '1'\n  parent_line: HeLa\n  plasmid_numbers: '1'\n  selectable_markers: neo\n  sequence: ''\n  species: human\n  genotype: ''\n"

		lines(:one).export_to_yaml.should eq yaml_str

	end

end