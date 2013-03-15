require 'test_helper'

class PlasmidTest < ActiveSupport::TestCase

  test "should link to its bacterial strain" do

		assert_equal(plasmids(:one).get_linked(:strainnumbers)["2"], bacteria(:two))

	end

	test "should have the number field set to the strain number" do

		assert_equal plasmids(:one).number_field.to_s, 1.to_s

	end

	test "should list exportable fields" do

		assert plasmids(:one).exportable_fields.size > 0

	end

	test "should name itself correctly" do

		assert_equal("#{Naming.name_for(Plasmid)}1", plasmids(:one).name_str)

	end

	test "should correctly set YAML export parameters" do

		e_p = plasmids(:one).get_export_params("yml")

		assert_equal("#{Naming.name_for(Plasmid)}1.yml", e_p[:filename])

	end

	test "should correctly set FASTA export parameters" do

		e_p = plasmids(:one).get_export_params("fasta")

		assert_equal("#{Naming.name_for(Plasmid)}1.fasta", e_p[:filename])

	end

	test "should correctly export to YAML" do

		yaml_str = "---\nPlasmid:\n  antibiotic: carb,kan,gent\n  concentration: '1.5'\n  date_entered: '2012-05-02'\n  description: CENP-I plasmid\n  enteredby: Colin\n  notebook: '1'\n  plasmidalias: CENP-I\n  plasmidmap: /plasmidmaps/original/missing.png\n  plasmidnumber: '1'\n  plasmidsize: '1'\n  sequence: ATTTGAGAGAAA\n  strainnumbers: '2'\n  vector: pCS2\n  verified: 'true'\n"

		assert_equal plasmids(:one).export_to_yaml, yaml_str

	end


	test "should correctly export to FASTA" do

		fasta_str = ">#{Naming.name_for(Plasmid)}1 CENP-I\nATTTGAGAGAAA"

		assert_equal plasmids(:one).export_to_fasta, fasta_str

	end

	test "should correctly parse its antibiotics" do

		plasmids(:one).parse_antibiotics

		[:carb, :kan, :gent].each do |a|
			assert_equal("1", plasmids(:one).send(a))
		end

		[:chlor, :tet, :strep].each do |a|
			assert_equal("0", plasmids(:one).send(a))
		end
	end

	test "should correctly calculate its size" do

		plasmids(:one).calculate_size

		assert_equal(plasmids(:one).sequence.length, plasmids(:one).plasmidsize)

	end

end
