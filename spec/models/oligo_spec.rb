require 'spec_helper'

describe Oligo do

	fixtures :oligos

  it "should not link objects" do
		oligos(:one).get_linked(:plasmid_number).should be_nil
	end

	it "should have the number field set to the oligo number" do
		oligos(:one).number_field.to_i.should eq 1
	end

	it "should list exportable fields" do
		oligos(:one).exportable_fields.should_not be_empty
	end

	it "should name itself correctly" do
		oligos(:one).name_str.should eq "#{Naming.name_for(Oligo)}1"
	end

	it "should correctly set YAML export parameters" do
		oligos(:one).get_export_params("yml")[:filename].should eq "#{Naming.name_for(Oligo)}1.yml"
	end

	it "should correctly set FASTA export parameters" do
		oligos(:one).get_export_params("fasta")[:filename].should eq "#{Naming.name_for(Oligo)}1.fasta"
	end

	it "should correctly export to YAML" do
		yaml_str = <<~YAML
      ---
      Oligo:
        date_entered: '2013-02-07'
        entered_by: Colin
        notebook: '1'
        oligo_number: '1'
        oligoalias: forward
        organism: Xenopus laevis
        purpose: cloning
        sequence: ATGGAGGAGAGGCCACCACAGAAAAGCC
        vendor: ''
      YAML

		oligos(:one).export_to_yaml.should eq yaml_str
	end

	it "should correctly export to FASTA" do
		fasta_str = ">#{Naming.name_for(Oligo)}1 forward\nATGGAGGAGAGGCCACCACAGAAAAGCC"
		oligos(:one).export_to_fasta.should eq fasta_str
	end
end
