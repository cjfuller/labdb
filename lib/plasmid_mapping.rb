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

  DEGENERATES = {"r" => "[ag]", "k" => "[gt]", "y" => "[ct]", "s" => "[cg]", "m" => "[ac]", "w" => "[at]", "n" => "[acgt]", "h" => "[act]", "v" => "[acg]", "b" => "[cgt]", "d" => "[agt]"}

  FEATURE_LIST = "lib/assets/known_sequence_features.yml"
  ENZYME_LIST = "lib/assets/restriction_enzymes.yml"
  #DEFAULT_ENZYME_LIST = "lib/assets/default_enzymes.yml"

  def self.load_all_known_features
    @known_features = YAML.load_file(FEATURE_LIST)
    @known_features.each do |name, info|
      seq = info['sequence']
      seq.downcase!
      info['sequence'] = Regexp.new(seq)
      info['size'] = seq.size
    end
    @known_enzymes = YAML.load_file(ENZYME_LIST)
    @known_enzymes.each do |name, info|
      seq = info['rec_seq']
      seq.downcase!
      DEGENERATES.each do |deg, repl|
        seq.gsub!(deg, repl)
      end
      info['rec_seq'] = Regexp.new(seq)
    end
  end

  def self.all_known_features
    @known_features
  end

  def self.all_known_enzymes
    @known_enzymes
  end

  def self.default_enzymes
    @default_enz
  end

  load_all_known_features

  def self.find_features(seq, feature_re, offset=0)
    seq.downcase!
    sites = []
    matches = seq.scan(feature_re) do |m|
      pos = Regexp.last_match.offset(0)[0] + offset + 1
      sites << pos unless pos > seq.length or sites.include? pos - seq.length/2
    end
    sites.map! { |s| s > seq.length/2 ? s-seq.length/2 : s }
    sites
  end

  def self.map_restriction_enzymes(map, seq)
    @known_enzymes.each do |name, info|
      sites = find_features(seq + seq, info['rec_seq'], info['cut_bef'])
      sites.each do |s|
        map.add_point_feature(
          PointFeature.new(name: name, pos: s,
            feature_class: 'restriction', total_count: sites.size))
      end
    end
  end

  def self.find_feature_exact(seq, feature_re)
    find_features(seq, feature_re)
  end

  def self.find_feature_inexact(seq, feature_re)
  end

  def self.translations(seq)
    unless seq and seq.length > 0 then
      return ["", "", ""]
    end
    seq = Bio::Sequence::NA.new(seq*2) #double the sequence to avoid end effects
    (1..3).map { |i| seq.translate(i).to_s }
  end

  def self.map_other_features(map, seq)
    trs = translations(seq)
    @known_features.each do |f, info|
      if info['type'] == :protein then
        trs.each_with_index do |tr, tr_i|
          unless (pos = find_feature_exact(tr, info['sequence'])).empty?
            pos.each do |p|
              map.add_regional_feature(
                RegionalFeature.new(
                  name: f,
                  start_pos: (p-1)*3 + tr_i + 1,
                  length: info['size']*3,
                  feature_class: (info['display']['feature_class'] or "affinity-tag")))
            end
          end
        end
      else
        unless (pos = find_feature_exact(seq, info['sequence'])).empty?
          pos.each do |p|
            map.add_regional_feature(
              RegionalFeature.new(
                name: f,
                start_pos: p,
                length: info['size'],
                color: (info['display']['feature_class'] or "affinity-tag")))
          end
        end
      end
    end
  end

  def self.map_for_plasmid(plas)
    plas_map = PlasmidMap.new(plas.named_number_string, plas.sequence.size)
    plas_map.size = plas.sequence.length
    plas_map.name = plas.named_number_string
    seq = (plas.sequence or "")

    map_restriction_enzymes(plas_map, seq)
    map_other_features(plas_map, seq)

    plas_map

  end

  class PlasmidMap

    def initialize(name, size)
      @name = name
      @size = size
      @regions = []
      @points = []
    end

    attr_accessor :size, :name

    def to_h
      pts = {}
      reg = {}
      @points.each do |p|
        pts[p.name] ||= []
        pts[p.name] << p.to_h
      end
      @regions.each do |r|
        reg[r.name] ||= []
        reg[r.name] << r.to_h
      end

      {pl_name: @name, pl_size: @size, point_features: pts, regional_features: reg}
    end

    def plasmidmap_json
      to_h.to_json
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

    def initialize(pos: 0, name: "", feature_class: "restriction", total_count: 1)
      @pos = pos
      @name = name
      @feature_class = feature_class
      @total_count = total_count
    end

    attr_accessor :pos, :name, :feature_class

    def to_h
      {type: 'point', at: @pos, text: @name, feature_class: @feature_class, count: @total_count}
    end

  end

  class RegionalFeature

    def initialize(start_pos: 0, length: 0, name: "", feature_class: "affinity-tag")
      @start = start_pos
      @length = length
      @name = name
      @feature_class = feature_class
    end

    attr_accessor :start, :length, :name, :feature_class

    def to_h
      {type: 'regional', start: @start, length: @length, text: @name, feature_class: @feature_class}
    end

  end

end
