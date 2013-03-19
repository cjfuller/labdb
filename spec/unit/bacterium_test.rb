require 'test_helper'

class BacteriumTest < ActiveSupport::TestCase

  test "should link to its plasmid" do

		assert_equal(bacteria(:one).get_linked(:plasmid_number)["2"], plasmids(:two))

	end

	test "should have the number field set to the strain number" do

		assert_equal bacteria(:one).number_field.to_s, 1.to_s

	end

	test "should list exportable fields" do

		assert bacteria(:one).exportable_fields.size > 0

	end

	test "should name itself correctly" do

		assert_equal("#{Naming.name_for(Bacterium)}1", bacteria(:one).name_str)

	end

	test "should correctly set YAML export parameters" do

		e_p = bacteria(:one).get_export_params("yml")

		assert_equal("#{Naming.name_for(Bacterium)}1.yml", e_p[:filename])

	end

	test "should correctly export to YAML" do

		yaml_str = "---\nBacterium:\n  comments: strain 1\n  date_entered: '2013-03-02'\n  entered_by: Colin\n  genotype: ''\n  notebook: '1'\n  plasmid_number: '2'\n  species_bkg: E. coli DH5a\n  strain_number: '1'\n  sequence: ''\n  strainalias: ''\n"

		assert_equal bacteria(:one).export_to_yaml, yaml_str

	end

end


