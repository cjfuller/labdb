###
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
###


class RestrictionEnzymeData

  @re_data_id = "#enzyme-data"
  @re_data_attr = "enz_data"
  @default_re_attr = "default_enz"

  @enzymes: () ->
    (n for n, info of @data)

  @default_enzymes: () ->
    @defaults

  @read_data: (json_data) ->
    @data = JSON.parse(json_data)

  @get: (name) ->
    return @data[name] || null

  @cuts_before: (name) ->
    return this.get(name) && @data[name]["cut_bef"]

  @seq: (name) ->
    return this.get(name) && @data[name]["rec_seq"]

  @read_from_page: () ->
    @read_data($(@re_data_id).attr(@re_data_attr))

  @read_defaults_from_page: () ->
    @defaults = JSON.parse($(@re_data_id).attr(@default_re_attr))

  @node_load_re_data: () ->
    fs = require 'fs'
    this.read_data(fs.readFileSync('../resources/restriction_enzymes.json'))


class FeatureData

  @feature_id = "#regional-features"

  @feature_attr = "feature_data"

  @read_data: (json_data) ->
    @data = JSON.parse(json_data)

  @get: (name) =>
    return @data[name] || null

  @feature_type: (name) =>
    return this.get(name) && @data[name]["type"]

  @display: (name) => 
    return this.get(name) && @data[name]["display"]

  @sequence: (name) =>
    return this.get(name) && @data[name]["sequence"]

  @exact: (name) =>
    return this.get(name) && @data[name]["exact"]

  @read_from_page: () =>
    @read_data($(@feature_id).attr(@feature_attr))

class Alignments

  @find_exact_matches: (seq_to_match, seq_in_which_to_match) ->
    match_indices = []
    exp = new RegExp(seq_to_match, "g")
    while (match = exp.exec(seq_in_which_to_match))
      match_indices.push(match.index)
    match_indices

class Plasmid

  @degenerate_sequence_translations = {
    "r" : "[ag]",
    "k" : "[gt]",
    "y" : "[ct]",
    "s" : "[cg]",
    "m" : "[ac]",
    "w" : "[at]",
    "n" : "[acgt]",
    "h" : "[act]",
    "v" : "[acg]",
    "b" : "[cgt]",
    "d" : "[agt]"
  }

  @codon_table = {"aaa":"K","aat":"N","aag":"K","aac":"N","ata":"I","att":"I","atg":"M","atc":"I","aga":"R","agt":"S","agg":"R","agc":"S","aca":"T","act":"T","acg":"T","acc":"T","taa":"*","tat":"Y","tag":"*","tac":"Y","tta":"L","ttt":"F","ttg":"L","ttc":"F","tga":"*","tgt":"C","tgg":"W","tgc":"C","tca":"S","tct":"S","tcg":"S","tcc":"S","gaa":"E","gat":"D","gag":"E","gac":"D","gta":"V","gtt":"V","gtg":"V","gtc":"V","gga":"G","ggt":"G","ggg":"G","ggc":"G","gca":"A","gct":"A","gcg":"A","gcc":"A","caa":"Q","cat":"H","cag":"Q","cac":"H","cta":"L","ctt":"L","ctg":"L","ctc":"L","cga":"R","cgt":"R","cgg":"R","cgc":"R","cca":"P","cct":"P","ccg":"P","ccc":"P"}

  @sub_degenerate_sequences: (seq) ->
    seq = seq.toLowerCase()
    for degen, repl of @degenerate_sequence_translations
      seq = seq.replace(new RegExp(degen, "g"), repl)
    seq

  @translate: (seq) ->
    seq = seq.toLowerCase()
    trans = ""
    codon_re = /.../g
    while (match = codon_re.exec(seq))
      trans+= @codon_table[match[0]] || "?"
      codon_re.lastIndex=(trans.length*3)
    trans

  constructor: (@name, @sequence) ->

    @point_features = []
    @regional_features = []
    @sequence = @sequence.toLowerCase()

  pl_size: () ->
    @sequence.length

  add_point_feature: (feature) ->
    existing = @point_features.filter((el, ind, arr) -> 
      el.name == feature.name && el.pos == feature.pos )
    if existing.length == 0 && feature.loc() <= @sequence.length
      @point_features.push(feature)

  remove_point_features: (name) ->
    to_remove = []
    for i in [0...@point_features.length]
      if @point_features[i].name == name
        to_remove.push(i)
    n_removals = 0
    for i in to_remove
      @point_features.splice(i-n_removals, 1)
      n_removals += 1


  add_regional_feature: (feature) ->
    existing = @regional_features.filter((el, ind, arr) -> 
      el.name == feature.name && el.start == feature.start && el.end == feature.end )
    if existing.length == 0 && feature.start <= @sequence.length
      @regional_features.push(feature)

  translate: (frame) -> #frame is 0, 1, or 2
    Plasmid.translate(@sequence.substr(frame) + @sequence)

  find_regional_features: (feature_table) ->

    frames = [this.translate(0), this.translate(1), this.translate(2)]

    for name, info of feature_table.data
      unless feature_table.feature_type(name) == "protein"
        continue
      for i in [0...frames.length]
        fr = frames[i]
        matches = Alignments.find_exact_matches(feature_table.sequence(name), fr)
        for match in matches
          info["display"] = info["display"] || {"color" : "black"}
          this.add_regional_feature(new RegionalFeature(name, info["display"], (match*3 + i) % @sequence.length, ((match + info["sequence"].length)*3 + 2 + i) % @sequence.length ))

  find_cut_sites: (enz_seq, cut_pos) ->
    unless enz_seq && cut_pos
      return []
    double_seq = @sequence + @sequence
    seq_size = @sequence.length
    enz_seq = Plasmid.sub_degenerate_sequences(enz_seq)
    enz_re = new RegExp(enz_seq, "g")
    matches = Alignments.find_exact_matches(enz_seq, double_seq)
    matches.filter((el, ind, arr) -> matches.indexOf(el - seq_size) < 0)

  map_restriction_enzymes: (enzyme_data, enzyme_list_to_map, unique_only=false) ->

    for enz in enzyme_list_to_map
      sites = this.find_cut_sites(enzyme_data.seq(enz), enzyme_data.cuts_before(enz))
      if unique_only && sites.length > 1
        continue
      for s in sites
        this.add_point_feature(new PointFeature(enz, {"color" : "red"}, s))


class Feature

  constructor: (@name, @display) -> 

  loc: () ->
    0

  text: () ->
    @name + " (#{@loc()})"

class PointFeature extends Feature

  constructor: (@name, @display, @pos) ->

  loc: () ->
    @pos


class RegionalFeature extends Feature

  constructor: (@name, @display, @start, @end) ->

  loc: () =>
    (@start + @end)/2

  text: () ->
    @name + " (#{@start} - #{@end})"
    

class PlasmidMap

  @plas_map_div_id = "#plasmid-map"
  @seq_attr = "sequence"
  @name_attr = "name"
  @dyn_enz_field = "#enzyme"
  @enz_remove_btn = "#hide_enzyme"
  @enz_add_btn = "#show_enzyme"

  @clear_maps: () ->
    d3.select(".chart").remove()

  init_svg: () =>
    @svg = d3.select(PlasmidMap.plas_map_div_id).append("div")
    .attr("class", "chart")
    .style("width", "#{@width}px")
    .style("height", "#{@height}px")
    .append("svg")
      .attr("width", @width).attr("height", @height)
    .append("g").attr("transform", "translate(#{@width/2},#{@height/2})")

  reinit_svg: () =>
    PlasmidMap.clear_maps()
    this.init_svg()

  constructor: () ->

    @width = 500
    @height = 400
    @map_radius = @width / 4
    @map_cx = 0
    @map_cy = 0
    @arc_width = @width / 96
    @spacer_width = 10
    @arc_outer_rad = @map_radius + @arc_width
    @arc_inner_rad = @map_radius - @arc_width
    @min_angular_dist = Math.PI/16
    @max_label_in_place_char_count = 25

    RestrictionEnzymeData.read_from_page()
    RestrictionEnzymeData.read_defaults_from_page()
    FeatureData.read_from_page()

    @plas = new Plasmid($(PlasmidMap.plas_map_div_id).attr(PlasmidMap.name_attr), $(PlasmidMap.plas_map_div_id).attr(PlasmidMap.seq_attr))
    @plas.map_restriction_enzymes(RestrictionEnzymeData, RestrictionEnzymeData.default_enzymes(), true)
    @plas.find_regional_features(FeatureData)

    $(PlasmidMap.enz_remove_btn).click(this.remove_enzyme_with_field) #TODO: check if this ends up bound to the correct object
    $(PlasmidMap.enz_add_btn).click(this.add_enzyme_with_field)

    @arc_templ = d3.svg.arc().outerRadius(@arc_outer_rad).innerRadius(@arc_inner_rad).startAngle(this.start_angle).endAngle(this.end_angle)
    @marker_templ = d3.svg.line.radial().radius(this.marker_radius).angle(this.feature_angle)

    @large_feature_groups = []

    this.init_svg()

  add_enzyme_with_field: =>
    this.add_enzyme($(PlasmidMap.dyn_enz_field).val())

  remove_enzyme_with_field: =>
    this.remove_enzyme($(PlasmidMap.dyn_enz_field).val())

  add_enzyme: (enz) =>
    @plas.map_restriction_enzymes(RestrictionEnzymeData, [enz], false)
    this.reinit_svg()
    this.render_drawing()

  remove_enzyme: (enz) =>
    @plas.remove_point_features(enz)
    this.reinit_svg()
    this.render_drawing()

  plas_data: () =>
    [@plas]

  features: () =>
    @plas.regional_features

  markers: () =>
    @plas.point_features

  combine_alternating = (arr0, arr1) ->
    result = []
    result.push([arr0[i], arr1[i]]) for i in [0...(arr0.length)]
    result

  markers_combined: () =>
    combine_alternating(this.markers(), this.markers())

  angle_from_bp: (bp) =>
    2*Math.PI*bp/this.plas_data()[0].pl_size()

  start_angle: (d, i) =>
    this.angle_from_bp(d.start)

  end_angle: (d, i) =>
    this.angle_from_bp(d.end)

  mid_angle: (d,i) =>
    (this.end_angle(d, i) + this.start_angle(d, i))/2

  feature_angle: (d, i) =>
    this.angle_from_bp(d.loc())

  marker_radius: (d, i) =>
    if i % 2 == 0
      @arc_inner_rad
    else
      @arc_outer_rad

  marker_label_x: (d, i) =>
    1.08*this.marker_radius(d,1) * Math.sin(d["angle"]) #no, not a mistake; weird coordinate system

  marker_label_y: (d, i) =>
    -1.08*this.marker_radius(d, 1) * Math.cos(d["angle"]) #see above

  plasmid_text: (d, i) ->
    d.name + " (#{d.pl_size()}bp)"

  feature_label_x: (d, i) =>
    1.08*@arc_outer_rad * Math.sin(this.mid_angle(d, i))

  feature_label_y: (d, i) =>
    -1.08*@arc_outer_rad * Math.cos(this.mid_angle(d, i))

  offset_loc = (angle) ->
    while angle < 0
      angle += 2*Math.PI
    if angle <= 2*Math.PI/16
      return "middle"
    else if angle > 2*Math.PI/16 && angle < 14*Math.PI/16
      return "start"
    else if angle >= 14*Math.PI/16 && angle < 18*Math.PI/16
      return "middle"
    else if angle >= 18*Math.PI/16 && angle < 30*Math.PI/16
      return "end"
    else
      return "middle"

  feature_offset_loc: (d, i) =>
    offset_loc(this.mid_angle(d, i))

  marker_offset_loc: (d, i) =>
    offset_loc(d["angle"])

  calculate_feature_angles: (features) =>
    (this.feature_angle(d, 0) for d in features)

  add_large_feature_group: (text) =>
    index = @large_feature_groups.length
    @large_feature_groups.push({text: text, index: index})
    @large_feature_groups.length - 1

  clear_large_feature_groups: () =>
    @large_feature_groups = []


  make_overlapping_feature_groups: (features) =>

    unless features.length > 0
      return [] 
    
    angles = this.calculate_feature_angles(features)
    group_id = 0
    groups = [ [0] ]

    unless angles.length > 1
      return groups

    for i in [1...angles.length]
      if angles[i] - angles[i-1] < @min_angular_dist && angles[i] - angles[groups[group_id][[0]]] < @min_angular_dist
        groups[group_id].push(i)
      else
        groups.push([i])
        group_id += 1

    groups


  feature_label_sort_function = (a, b) ->
    if a.loc() < b.loc() 
      return -1
    if a.loc() > b.loc()
      return 1
    return 0    

  fix_overlapping_labels: (regional_features, point_features) =>

    all_features = regional_features.concat(point_features)
    
    all_features = all_features.sort(feature_label_sort_function)

    groups = this.make_overlapping_feature_groups(all_features)

    angles = this.calculate_feature_angles(all_features)

    group_labels = []

    this.clear_large_feature_groups()

    for group in groups
      label = all_features[group[0]].text()
      sum_angle = angles[group[0]]
      for i in [1...group.length]
        label += ", "
        label += all_features[group[i]].text()
        sum_angle += angles[group[i]]
      if label.length > @max_label_in_place_char_count
        large_group_id = this.add_large_feature_group(label)
        label = "[#{large_group_id+1}]"
      group_labels.push({angle: sum_angle/group.length, text: label})

    group_labels

  draw_circle: ->
    @svg.selectAll("circle")
      .data(this.plas_data())
      .enter().append("circle")
      .attr("cx", @map_cx)
      .attr("cy", @map_cy)
      .attr("r", @map_radius)
      .style("fill", "none").style("stroke", "#000")

  arc: =>
    @arc_templ

  draw_features: =>
    @svg.append("g").selectAll(".arc")
      .data(this.features())
      .enter().append("path").attr("class", "arc")
      .attr("d", @arc_templ)
      .style("fill", (d,i) -> d.display["color"] )

  draw_feature_and_marker_labels: =>
    new_labels = this.fix_overlapping_labels(this.features(), this.markers())
    label_group = @svg.append("g")
    label_group.selectAll(".label")
      .data(new_labels).enter().append("text")
      .attr("class", "feature-label")
      .attr("x", this.marker_label_x)
      .attr("y", this.marker_label_y)
      .text((d,i) -> d.text)
      .attr("text-anchor", this.marker_offset_loc)

  draw_markers: =>
    line_group = @svg.append("g")
    line_group.selectAll(".line")
      .data(this.markers_combined()).enter().append("path")
      .attr("class", "line")
      .attr("d", @marker_templ)
      .style("stroke", (d,i) -> d[0].display["color"] )

  draw_large_group_labels: =>
    all_large = @large_feature_groups
    all_large = all_large.sort((a,b) -> 
      if a['index'] < b['index']
        return -1
      if a['index'] > b['index']
        return 1
      return 0
    )
    label_group = @svg.append("g")
    label_group.selectAll(".cluster-label")
      .data(all_large).enter().append("text")
      .attr("class", "cluster-label")
      .attr("x", @spacer_width - @width/2)
      .attr("y", (d, i) => @height/2 - @spacer_width*(all_large.length - i))
      .text((d, i) -> "[#{d['index']+1}]: #{d['text']}")
      .attr("text-anchor", "start")


  draw_plas_label: =>
    center_label_group = @svg.append("g")
    center_label_group.selectAll(".plasmid-label")
      .data(this.plas_data()).enter().append("text")
        .attr("class", "plasmid-label")
        .text(this.plasmid_text)
        .attr("text-anchor", "middle")

  render_drawing: =>
    this.draw_circle()
    this.draw_features()
    this.draw_markers()
    this.draw_feature_and_marker_labels()
    this.draw_large_group_labels()
    this.draw_plas_label()

do_map = ->
  PlasmidMap.clear_maps()
  map = new PlasmidMap()
  map.render_drawing()

define_names = ->
  root = exports ? window
  root.plmp = 
    RestrictionEnzymeData : RestrictionEnzymeData
    FeatureData : FeatureData
    Alignments : Alignments
    Plasmid : Plasmid
    Feature : Feature
    PointFeature : PointFeature
    RegionalFeature : RegionalFeature 
    PlasmidMap : PlasmidMap

$("#show-map-button").on("click", do_map)

define_names()


