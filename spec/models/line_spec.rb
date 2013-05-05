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

		yaml_str = "---\nLine:\n  current_stock_counts: 1 (clone 2),3 (clone 7)\n  date_entered: '2013-04-15'\n  description: N-terminal GFP-CENP-A HeLa constitutive expressing\n  entered_by: Colin Fuller\n  line_alias: GFP-CENP-A HeLa\n  line_number: '1'\n  locations: lN2R1S1,lN2R1S2\n  notebook: '1'\n  parent_line: HeLa\n  plasmid_numbers: '1'\n  selectable_markers: neo\n  sequence: ''\n  species: human\n  genotype: ''\n  stock_person: Colin Fuller, Colin Fuller\n  stock_date: 20130415, 20130416\n"

		lines(:one).export_to_yaml.should eq yaml_str

	end

	it "should correctly generate inventory" do
		inv = lines(:one).inventory
		inv[0].location.should eq "lN2R1S1"
		inv[0].clone.should eq 2.to_s
		inv[0].count.should eq 1
		inv[0].date.should eq Date.parse("2013-04-15")
		inv[0].person.should eq "Colin Fuller"
	end

	it "should correctly generate inventory fields from an inventory" do
		inv = []
		inv << Line::InventoryItem.new
		inv << Line::InventoryItem.new

		loc = ["A", "B"]
		clone = ["1", "2"]
		count = [1, 2]
		date = [Date.parse("2000-01-01"), Date.parse("2000-01-02")]
		person = ["CJF", "CF"]

		inv.each_with_index do |e, i|

			e.location = loc[i]
			e.clone  = clone[i]
			e.count = count[i]
			e.date = date[i]
			e.person = person[i]

		end

		l = lines(:one)

		l.update_inventory(inv)

		l.current_stock_counts.should eq count.map.with_index { |c, i| "#{c}(clone #{clone[i]})" }.join(",")
		l.locations.should eq loc.join(",")
		l.stock_person.should eq person.join(",")
		l.stock_date.should eq date.join(",")

	end

end