require 'test_helper'

class AntibodyTest < ActiveSupport::TestCase

	test "should not link objects" do

		assert_nil antibodies(:one).get_linked(:plasmid_number)

	end

	test "should have the number field set to the antibody number" do

		assert_equal antibodies(:one).number_field.to_s, 1.to_s

	end

	test "should list exportable fields" do

		assert antibodies(:one).exportable_fields.size > 0

	end

	test "should name itself correctly" do

		assert_equal("#{Naming.name_for(Antibody)}1", antibodies(:one).name_str)

	end

	test "should correctly set YAML export parameters" do

		e_p = antibodies(:one).get_export_params("yml")

		assert_equal("#{Naming.name_for(Antibody)}1.yml", e_p[:filename])

	end

	test "should correctly export to YAML" do

		yaml_str = "---\nAntibody:\n  ab_number: '1'\n  alias: human anti-CENP-A\n  box: human\n  comments: Fix in methanol for best results.\n  entered_by: Colin\n  fluorophore: ''\n  good_for_if: 'true'\n  good_for_western: 'true'\n  host: rabbit\n  label: anti-CENP-A\n  vendor: Cocalico/straightlab\n  date_entered: '2013-03-08'\n"

		assert_equal antibodies(:one).export_to_yaml, yaml_str

	end

end
