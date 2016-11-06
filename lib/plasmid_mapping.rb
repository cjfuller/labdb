require 'json'
require 'yaml'

require 'bio'

module PlasmidMapping

  # Feature types -- these define what we look for, but don't have an actual
  # location in the plasmid yet.  We turn these into actual features, which are
  # a sequence match of one of the prototypes + a position.
  module FeaturePrototype
    attr_reader :name, :sequence, :exact, :display

    # target_sequence is expected to be already doubled to avoid edge effects.
    def locate_all_in(target_sequence)
      all_match_pos = []
      target_sequence
        .downcase
        .scan(Regexp.new(self.sequence)) { all_match_pos << Regexp.last_match.offset(0)[0] }
      all_match_pos
        .select { |s| s <= target_sequence.length / 2 }
        .map { |s| feature_from_position(s) }
    end
  end

  Protein = Struct.new(:name, :sequence, :exact, :display) do
    include FeaturePrototype
    def feature_extent() :regional end
    def feature_from_position(pos)
      ProteinSequence.new(self, pos + 1, self.sequence.length)
    end
  end

  Protease = Struct.new(:name, :sequence, :exact, :display) do
    include FeaturePrototype
    def feature_extent() :point end
    def feature_from_position(pos)
      ProteaseSite.new(self, pos + 1)
    end
  end

  RestrictionEnzyme = Struct.new(:name, :sequence, :display, :cuts_before) do
    include FeaturePrototype
    attr_reader :exact

    def initialize(*args)
      super(*args)
      self.display ||= {}
      @exact = true
    end

    def feature_extent() :point end
    def feature_from_position(pos)
      RestrictionSite.new(self, pos + 1 + self.cuts_before)
    end
  end

  # Concrete features - a prototype + position
  Feature = Struct.new(:prototype, :pos) do
    def to_h
      {
        pos: self.pos,
        feature: self.prototype.to_h,
        featureClass: self.feature_class,
        featureExtent: self.prototype.feature_extent,
      }
    end
  end

  module RegionalFeature
    attr_accessor :feature_length
    def to_h
      super().merge({length: self.feature_length})
    end
  end

  class RestrictionSite < Feature
    def feature_class
      'restriction'
    end
  end

  class ProteaseSite < Feature
    def feature_class
      'protease'
    end
  end

  class ProteinSequence < Feature
    include RegionalFeature
    def initialize(prototype, pos, feature_length)
      super(prototype, pos)
      self.feature_length = feature_length
    end

    def feature_class
      'protein'
    end
  end

  DEGENERATES = {
    "r" => "[ag]",
    "k" => "[gt]",
    "y" => "[ct]",
    "s" => "[cg]",
    "m" => "[ac]",
    "w" => "[at]",
    "n" => "[acgt]",
    "h" => "[act]",
    "v" => "[acg]",
    "b" => "[cgt]",
    "d" => "[agt]",
  }

  FEATURE_LIST = "lib/assets/known_sequence_features.yml"
  ENZYME_LIST = "lib/assets/restriction_enzymes.yml"

  def self.load_all_known_features
    @known_features = YAML.load_file(FEATURE_LIST).map do |name, info|
      if info['display']['feature_class'] == 'protease'
        Protease.new(name, info['sequence'].downcase, info['exact'], info['display'])
      else
        Protein.new(name, info['sequence'].downcase, info['exact'], info['display'])
      end
    end
    @known_enzymes = YAML.load_file(ENZYME_LIST).map do |name, info|
      degen_seq = info['rec_seq'].downcase
      seq = DEGENERATES.reduce(degen_seq) { |curr_seq, degen| curr_seq.gsub(*degen) }
      RestrictionEnzyme.new(name, seq, info['display'], info['cut_bef'])
    end
  end

  def self.all_known_features
    @known_features
  end

  def self.all_known_enzymes
    @known_enzymes
  end

  load_all_known_features

  def self.find_features(seq, feature_re, offset=0)
  end

  def self.map_restriction_enzymes(seq)
    @known_enzymes.flat_map { |e| e.locate_all_in(seq * 2) }
  end

  def self.translations(seq)
    unless seq and seq.length > 0 then
      return ["", "", ""]
    end
    seq = Bio::Sequence::NA.new(seq*2) #double the sequence to avoid end effects
    (1..3).map { |i| seq.translate(i).to_s }
  end

  def self.map_other_features(seq)
    translations(seq).flat_map.with_index do |tr, frame|
      features = @known_features.flat_map do |f|
        # TODO(colin): don't assume all these have to be proteins.
        raise 'Assertion failed: feature must be a protein' unless f.is_a?(Protein) || f.is_a?(Protease)
        f.locate_all_in(tr)
      end
      # features now has 1-indexed positions in terms of protein sequence, and
      # we need them in terms of DNA sequence: subtract 1, multiply by 3,
      # add 1 + frame offset
      features.map do |f|
        f2 = f.clone
        f2.pos = (f2.pos - 1) * 3 + 1 + frame
        if f2.respond_to? :feature_length
          f2.feature_length = f2.feature_length * 3
        end
        f2
      end
    end
  end

  def self.map_for_plasmid(seq)
    map_restriction_enzymes(seq) + map_other_features(seq)
  end
end
