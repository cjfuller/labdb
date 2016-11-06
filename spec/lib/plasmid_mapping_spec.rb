require 'spec_helper'
require 'plasmid_mapping'

describe PlasmidMapping do
  Sequence = File.read('spec/fixtures/mapping.txt').strip

  it 'should locate restriction enzymes in a DNA sequence' do
    sites = PlasmidMapping.map_restriction_enzymes(Sequence)
    first_pos = sites.select { |s| s.prototype.name == "NdeI" }.first.pos 
    expect(first_pos).to eq 5148
  end

  it 'should locate protein features in a DNA sequence' do
    sites = PlasmidMapping.map_other_features(Sequence)
    hisSites = sites.select { |s| s.prototype.name == "6xHis" }
    expect(hisSites.first.pos).to eq 5083
    expect(hisSites.first.feature_length).to eq 18
    expect(hisSites[1].pos).to eq 5206
    expect(hisSites[1].feature_length).to eq 18
  end
end
