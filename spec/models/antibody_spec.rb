require 'spec_helper'

describe Antibody do

	fixtures :antibodies

	it "should not link objects" do
		antibodies(:one).get_linked(:plasmid_number).should be_nil
	end

	it "should have the number field set to the antibody number" do
		antibodies(:one).number_field.to_i.should eq 1
	end

	it "should list exportable fields" do
		antibodies(:one).exportable_fields.should_not be_empty
	end

	it "should name itself correctly" do
		antibodies(:one).name_str.should eq "#{Naming.name_for(Antibody)}1"
	end

	it "should correctly set YAML export parameters" do
		antibodies(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Antibody)}1.yml"
	end

	it "should correctly export to YAML" do

		yaml_str = <<~YAML
      ---
      Antibody:
        ab_number: '1'
        alias: human anti-CENP-A
        box: human
        comments: Fix in methanol for best results.
        date_entered: '2013-03-08'
        entered_by: Colin
        fluorophore: ''
        good_for_if: 'true'
        good_for_western: 'true'
        host: rabbit
        label: anti-CENP-A
        vendor: Cocalico/straightlab
      YAML

		antibodies(:one).export_to_yaml.should eq yaml_str
	end
end
