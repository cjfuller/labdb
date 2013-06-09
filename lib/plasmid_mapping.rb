#--
# Copyright (C) 2013  Colin J. Fuller
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#++

require 'json'
require 'yaml'

require 'bio'

module PlasmidMapping

  FEATURE_LIST = "lib/assets/known_sequence_features.yml"

  def self.load_all_known_features
    @known_features = YAML.load_file(FEATURE_LIST)
  end

  def self.all_known_features
    @known_features
  end

  load_all_known_features

  DEGENERATES = {"r" => "[ag]", "k" => "[gt]", "y" => "[ct]", "s" => "[cg]", "m" => "[ac]", "w" => "[at]", "n" => "[acgt]", "h" => "[act]", "v" => "[acg]", "b" => "[cgt]", "d" => "[agt]"}

  def self.find_cut_sites(seq, enz_seq, cut_pos)
    seq.downcase!
    enz_seq.downcase!
    DEGENERATES.each do |deg, repl|
      enz_seq.gsub!(deg, repl)
    end

    enz_re = Regexp.new(enz_seq)

    sites = []

    matches = seq.scan(enz_re) do |m|
      pos = Regexp.last_match.offset(0)[0] + cut_pos + 1
      sites << pos unless pos > seq.length or sites.include? pos - seq.length/2
    end

    sites

  end

  def self.map_restriction_enzymes(map, seq)

    ["AscI", "PacI", "EcoRI", "XbaI", "NdeI", "BamHI"].each do |name|
      enz = Bio::RestrictionEnzyme.rebase[name]
      sites = find_cut_sites(seq, enz[:pattern], enz[:primary_strand_cut1])

      sites.each do |s|
        f = PointFeature.new
        f.name = name
        f.pos = s
        f.color = "red"
        map.add_point_feature(f)
      end

    end

  end

  def self.map_for_plasmid(plas)
    plas_map = PlasmidMap.new
    plas_map.size = plas.plasmidsize
    plas_map.name = plas.named_number_string

    seq = Bio::Sequence::NA.new(plas.sequence*2) #double the sequence to avoid end effects
    frames = (1..3).map { |i| seq.translate(i) }
    all_known_features.each do |feat|
      if feat[:seq_type] == :protein then
        feat_exp = Regexp.new(feat[:seq])
        m_objs = frames.map { |fr| feat_exp.match(fr) }

        starts = m_objs.map do |m_obj|
          if m_obj then
            if feat[:feature_type] = :point then

            elsif feat[:feature_type] = :regional then
              f = RegionalFeature.new
              f.start = m_obj.offsets(0)[0]
              f.end = m_obj.offsets(0)[1]
              if f.end > seq.length/2 then
                f.end -= seq.length/2
              end
              f.name = feat[:name]
              f.color = feat[:color] || "black"
              plas_map.add_regional_feature(f)
            end
          end
        end

      end
    end

    map_restriction_enzymes(plas_map, seq)

    plas_map

  end

  class PlasmidMap

    def initialize
      @size = 3500
      @name = "ASP1"
      @regions = []
      @points = []
    end 

    attr_accessor :size, :name

    def to_h
      {pl_name: @name, pl_size: @size}
    end

    def plasmidmap_json
      [to_h].to_json
    end

    def regional_json
      @regions.map(&:to_h).to_json
    end

    def point_json
      @points.map(&:to_h).to_json
    end

    def add_point_feature(m)
      @points << m
      self
    end

    def add_regional_feature(f)
      @regions << f
      self
    end

  end

  class PointFeature

    def initialize
      @pos = 0
      @name = "Ori"
      @color = "red"
    end

    attr_accessor :pos, :name, :color

    def to_h
      {at: @pos, text: @name, color: @color}
    end

  end

  class RegionalFeature

    def initialize
      @start = 50
      @end = 1200
      @name = "GFP"
      @color = "green"
    end

    attr_accessor :start, :end, :name, :color

    def to_h
      {start: @start, end: @end, text: @name, color: @color}
    end

  end

end

