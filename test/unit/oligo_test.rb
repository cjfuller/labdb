require 'test_helper'
require 'object_naming'

class OligoTest < ActiveSupport::TestCase

  test "should not link objects" do

		assert_nil oligos(:one).get_linked(:plasmid_number)

	end

	test "should have the number field set to the oligo number" do

		assert_equal oligos(:one).number_field.to_s, 1.to_s

	end

	test "should list exportable fields" do

		assert oligos(:one).exportable_fields.size > 0

	end

	test "should name itself correctly" do

		assert_equal("#{Naming.name_for(Oligo)}1", oligos(:one).name_str)

	end

	test "should correctly set YAML export parameters" do

		e_p = oligos(:one).get_export_params("yml")

		assert_equal("#{Naming.name_for(Oligo)}1.yml", e_p[:filename])

	end

	test "should correctly set FASTA export parameters" do

		e_p = oligos(:one).get_export_params("fasta")

		assert_equal("#{Naming.name_for(Oligo)}1.fasta", e_p[:filename])

	end

	test "should correctly export to YAML" do

		yaml_str = "---\nOligo:\n  oligoalias: forward\n  date_entered: '2013-02-07'\n  entered_by: Colin\n  notebook: '1'\n  oligo_number: '1'\n  organism: X. laevis\n  purpose: cloning\n  sequence: ATGGAGGAGAGGCCACCACAGAAAAGCC\n  vendor: ''\n"

		assert_equal oligos(:one).export_to_yaml, yaml_str

	end


	test "should correctly export to FASTA" do

		fasta_str = ">#{Naming.name_for(Oligo)}1 forward\nATGGAGGAGAGGCCACCACAGAAAAGCC"

		assert_equal oligos(:one).export_to_fasta, fasta_str

	end

end
