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

class Plasmid

  constructor: (@name, @size) ->

    @point_features = []
    @regional_features = []

  pl_size: () ->
    @size

  add_point_feature: (feature) ->
    existing = @point_features.filter((el, ind, arr) ->
      el.name == feature.name && el.pos == feature.pos )
    if existing.length == 0 && feature.loc() <= this.pl_size()
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


  add_regional_feature: (feature) =>
    existing = @regional_features.filter((el, ind, arr) ->
      el.name == feature.name && el.start == feature.start && el.end == feature.end)
    if existing.length == 0 && feature.start <= this.pl_size()
      @regional_features.push(feature)

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

  @initially_displayed_enzymes = ["AscI", "PacI", "EcoRI", "BamHI"]

  @plas_map_div_id = "#plasmid-map"
  @data_attr = "data"
  @size_attr = "pl_size"
  @name_attr = "pl_name"
  @point_attr = "point_features"
  @regional_attr = "regional_features"
  @dyn_enz_field = "#enzyme"
  @enz_remove_btn = "#hide_enzyme"
  @enz_add_btn = "#show_enzyme"

  @f_text = "text"
  @f_color = "color"
  @f_start = "start"
  @f_length = "length"
  @f_pos = "at"

  @clear_maps: () ->
    d3.select(".chart").remove()

  init_svg: () =>
    @svg = d3.select(PlasmidMap.plas_map_div_id).append("div")
    .attr("class", "chart")
    .style("width", "#{@width}px")
    .style("height", "#{@height}px")
    .append("svg")
      .attr("width", @width).attr("height", @height)
    .append("g").attr("transform", "translate(#{@width/2 + @map_offset_x},#{@height/2 + @map_offset_y})")

  reinit_svg: () =>
    PlasmidMap.clear_maps()
    this.init_svg()

  constructor: () ->

    @width = 500
    @height = 500
    @map_radius = @width / 4
    @map_cx = 0
    @map_cy = 0
    @map_offset_x = 0
    @map_offset_y = -100
    @arc_width = @width / 96
    @spacer_width = 10
    @arc_outer_rad = @map_radius + @arc_width
    @arc_inner_rad = @map_radius - @arc_width
    @min_angular_dist = Math.PI/16
    @max_label_in_place_char_count = 14

    @data = JSON.parse($(PlasmidMap.plas_map_div_id).attr(PlasmidMap.data_attr))

    @plas = new Plasmid(@data[PlasmidMap.name_attr], @data[PlasmidMap.size_attr])
    
    for name, f_list of @data['point_features']
      if f_list.length == 1 or name in PlasmidMap.initially_displayed_enzymes
        for f in f_list
          @plas.add_point_feature(
            new PointFeature(f['text'], {'color' : f['color']}, f['at']))

    for name, f_list of @data['regional_features']
      for f in f_list
        @plas.add_regional_feature(
          new RegionalFeature(f['text'], {'color' : f['color']}, f['start'], f['start'] + f['length'] - 1))

    $(PlasmidMap.enz_remove_btn).click(this.remove_enzyme_with_field)
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
    unless (@data['point_features'][enz] == undefined)
      for f in @data['point_features'][enz]
        @plas.add_point_feature(new PointFeature(f['text'], {'color' : f['color']}, f['at']))
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
    index = 0
    if @large_feature_groups.length > 0
      index = @large_feature_groups[@large_feature_groups.length-1]['group_index'] + 1
    new_offset = 0
    if index > 0
      new_offset = @large_feature_groups.length
    parts = text.split(", ")
    chunks = []
    max_num_per_chunk = 5
    if parts.length > max_num_per_chunk
      i = 0
      while i < parts.length
        chunks.push(parts[i...(i+max_num_per_chunk)].join(", "))
        i += max_num_per_chunk
    else
      chunks.push(text)
    
    for c,i in chunks
      if i == 0
        @large_feature_groups.push({text: c, index: index, group_index: index})
      else
        @large_feature_groups.push({text: c, index: -1, group_index: index})

    index

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

  large_feature_group_label_offset_y: (d,i) =>
    @height/2 - @spacer_width*(@large_feature_groups.length - i) - @map_offset_y

  large_feature_group_label_offset_x: (d,i) =>
    extra_offset = 0
    if d['index'] == -1
      extra_offset = 15
    @spacer_width - @width/2 - @map_offset_x + extra_offset


  large_group_label_text: (d,i) ->
    t = ""
    if d['index'] == -1
      t = "#{d['text']}"
    else
      t = "[#{d['index']+1}] #{d['text']}"
    t


  draw_large_group_labels: =>
    label_group = @svg.append("g")

    label_group.selectAll(".cluster-label")
      .data(@large_feature_groups).enter().append("text")
      .attr("class", "cluster-label")
      .attr("x", this.large_feature_group_label_offset_x)
      .attr("y", this.large_feature_group_label_offset_y)
      .text(this.large_group_label_text)
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
    Plasmid : Plasmid
    Feature : Feature
    PointFeature : PointFeature
    RegionalFeature : RegionalFeature 
    PlasmidMap : PlasmidMap

$("#show-map-button").on("click", do_map)

define_names()


