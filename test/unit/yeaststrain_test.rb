require 'test_helper'

class YeaststrainTest < ActiveSupport::TestCase

	test "should link to its plasmid" do

		assert_equal(yeaststrains(:one).get_linked(:plasmidnumber)["2"], plasmids(:two))

	end

	test "should have the number field set to the strain number" do

		assert_equal yeaststrains(:one).number_field.to_s, 1.to_s

	end

	test "should list exportable fields" do

		assert yeaststrains(:one).exportable_fields.size > 0

	end

	test "should name itself correctly" do

		assert_equal("#{Naming.name_for(Yeaststrain)}1", yeaststrains(:one).name_str)

	end

	test "should correctly set YAML export parameters" do

		e_p = yeaststrains(:one).get_export_params("yml")

		assert_equal("#{Naming.name_for(Yeaststrain)}1.yml", e_p[:filename])

	end

	test "should correctly set FASTA export parameters" do

		e_p = yeaststrains(:one).get_export_params("fasta")

		assert_equal("#{Naming.name_for(Yeaststrain)}1.fasta", e_p[:filename])

	end

	test "should correctly export to YAML" do

		yaml_str = "---\nYeaststrain:\n  antibiotic: ''\n  comments: ''\n  date_entered: '2013-03-02'\n  entered_by: Colin\n  genotype: leu1- ura4-\n  location: -80 S2R2\n  plasmidnumber: '2'\n  sequence: AATAAGAGAGC\n  species: pombe\n  strain_bkg: 972 h-\n  strain_number: '1'\n  strainalias: leu-/ura- h-\n  notebook: ''\n"

		assert_equal yeaststrains(:one).export_to_yaml, yaml_str

	end


	test "should correctly export to FASTA" do

		fasta_str = ">#{Naming.name_for(Yeaststrain)}1 leu-/ura- h-\nAATAAGAGAGC"

		assert_equal yeaststrains(:one).export_to_fasta, fasta_str

	end

end
