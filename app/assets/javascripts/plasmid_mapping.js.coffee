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

parameters = {
  plas_map_div_id: "#plasmid-map",
  data_attr: "data",
  dyn_enz_field: "#enzyme",
  enz_remove_button: "#hide_enzyme",
  enz_add_button: "#show_enzyme",

  map_width: 500,
  map_height: 400,
  map_offset_x: 0,
  map_offset_y: 35,
  info_offset_y: 0,
  spacer_width: 10,

  min_angular_dist: Math.PI/32,
  max_label_in_place_char_count: 14,

  initially_displayed_enzymes: ['AscI', 'PacI'],
  initially_display_single_cutters: true,
}

parameters.map_radius = parameters.map_width/4
parameters.arc_width = parameters.map_width/96
parameters.inner_radius = parameters.map_radius - parameters.arc_width
parameters.outer_radius = parameters.map_radius + parameters.arc_width
parameters.display_height = parameters.map_height

objects = {
  features_displayed: {},
  features_not_displayed: {},
}

p = (name=undefined) ->
  if name
    parameters[name]
  else
    parameters

o = () ->
  objects

objects.point_features = () ->
  f_to_draw = o().features_displayed
  point_f_to_draw = []
  for k,v of f_to_draw
    if v[0].type == 'point'
      for vi in v
        point_f_to_draw.push(vi)
  return point_f_to_draw


# Functions to calculate various properties of features

angle_from_bp = (bp) ->
  2*Math.PI*bp/p('pl_size')

x_from_polar = (r, theta) ->
  r*Math.cos(theta - Math.PI/2)

y_from_polar = (r, theta) ->
  r*Math.sin(theta - Math.PI/2)

feature_angle = (d,i) ->
  angle_from_bp(d.at)

text_for_group = (g, i) ->
  strrep = ""
  for f in g
    if (strrep.length > 0)
      strrep += ", "
    else 
      strrep += "[#{i}]: "
    strrep += "#{f.text} (#{f.at})"
  strrep

calculate_map_offset = () ->
  if $('.alert').length > 0
    p().info_offset_y = -1*$('.alert').outerHeight(true)
  else
    p().info_offset_y = 0

update_svg_height = () ->
  $("svg").attr("height", p('display_height'))
  $(".chart").attr("style", "width: #{p('map_width')}px; height: #{p('display_height')}px;")

update_offset = () ->
  $("#offset-group").attr("transform", "translate(#{p('map_width')/2 + p('map_offset_x')}, #{p('map_height')/2 + p('map_offset_y') + p('info_offset_y')})")
  p().display_height = p().map_height + p().info_offset_y
  update_svg_height()

reset_point_group_highlight = () ->
  #TODO: factor this out into css classes instead of explicit styling
  $('.feature-group').attr('style', 'fill: black; stroke: black')


reset_point_group_info = () ->
  p().info_offset_y = 0
  update_offset()
  reset_point_group_highlight()
  $('.alert').remove()

# Drawing functions

do_expand_point_group = (event) ->
  $('.alert').remove()
  reset_point_group_highlight()
  gr_index = parseInt(event.target.getAttribute('group-index'))
  g = o().groups[gr_index]
  event.target.style = "stroke: black; fill: yellow;"
  info_el = document.createElement("div")
  info_el.className = "alert alert-info"
  $(p('plas_map_div_id')).prepend(info_el)
  $('.alert').append('<button type="button" class="close">&times;</button>')
  $('.alert').append(text_for_group(g, gr_index + 1))
  $('.alert').attr("style", "color: black;") #TODO: refactor into css
  $('button.close').click(reset_point_group_info)
  calculate_map_offset()
  update_offset()

calculate_text_anchor_point = (angle) ->
  if angle < Math.PI
    return "start"
  else
    return "end"

draw_circle = () ->
  o().svg.append("g")
    .selectAll("circle")
    .data([0])
    .enter().append("circle")
      .attr("cx", 0)
      .attr("cy", 0)
      .attr("r", p('map_radius'))
      .style("fill", "none")
      .style("stroke", "#000")

draw_point_features = () ->
  #TODO: use data
  grp = o().svg.append("g")
  for f in o().point_features()
    grp.append("path")
      .attr("class", "line")
      .attr("d", d3.svg.line.radial()([[p('inner_radius'), feature_angle(f)], [p('outer_radius'), feature_angle(f)]]))
      .style("stroke", f.color)

draw_point_groups = () ->
  #TODO: don't hard code radius
  #TODO: use data
  grp = o().svg.append("g")
  for g, i in o().groups
    grp.append("path")
      .attr("class", "arc feature-group")
      .attr("group-index", i)
      .attr("d", d3.svg.arc().outerRadius(p('outer_radius') + 6)
                  .innerRadius(p('outer_radius') + 2)
                  .startAngle(angle_from_bp(g[0].at) - Math.PI/256)
                  .endAngle(angle_from_bp(g[g.length - 1].at) + Math.PI/256))
      .style("stroke", "black")
      .style("fill", "black")
  $('.feature-group').mouseover(do_expand_point_group)


draw_single_feature_label = (grp, angle, text, radial_offset=3, text_rotation=true) ->
  symm_angle = angle
  if angle > Math.PI
    symm_angle = Math.abs(angle - Math.PI*2)
  rotation_angle = 0.25*(symm_angle - Math.PI/2)/Math.PI*180
  if not text_rotation
    rotation_angle = 0
  if angle > Math.PI
    rotation_angle *= -1
  x = x_from_polar(p('outer_radius') + radial_offset, angle)
  y = y_from_polar(p('outer_radius') + radial_offset, angle) + Math.sin(symm_angle/2)*8
  grp.append("text")
    .attr("class", "feature-label")
    .attr("x", x)
    .attr("y", y)
    .attr("text-anchor", calculate_text_anchor_point(angle))
    .text(text)
    .attr("transform", "rotate(#{rotation_angle}, #{x}, #{y})")

draw_point_labels = () ->
  grp = o().svg.append("g")
  for f in o().point_features()
    if f.grouped
      continue
    angle = angle_from_bp(f.at)
    text = "#{f.text} (#{f.at})"
    draw_single_feature_label(grp, angle, text)
    
draw_group_labels = () ->
  grp = o().svg.append("g")
  for g, i in o().groups
    angle = (angle_from_bp(g[0].at) + angle_from_bp(g[g.length - 1].at))/2
    text = "#{i+1}"
    draw_single_feature_label(grp, angle, text, 10, false)

initialize_map = () ->
  o().svg = d3.select(p('plas_map_div_id')).append("div")
    .attr("class", "chart")
    .style("width", "#{p('map_width')}px")
    .style("height", "#{p('display_height')}px")
    .append("svg")
      .attr("width", p('map_width'))
      .attr("height", p('display_height'))
    .append("g")
      .attr("id", "offset-group")
      .attr("transform", "translate(#{p('map_width')/2 + p('map_offset_x')}, #{p('map_height')/2 + p('map_offset_y') + p('info_offset_y')})")
  update_offset()

clear_map = () ->
  if d3.select(".chart")
    d3.select(".chart").remove()

draw_all = () ->
  fix_feature_overlap()
  calculate_map_offset()
  update_offset()
  initialize_map()
  update_offset()
  draw_circle()
  draw_point_features()
  draw_point_groups()
  draw_point_labels()
  draw_group_labels()

redraw_map = () ->
  clear_map()
  draw_all()


# Add/remove functions

ensure_n_feature_groups = (n) ->
  while o().groups.length < n
    o().groups.push([])

group_feature = (f, group_n) ->
  if group_n < 0
    f.grouped = false
    return
  ensure_n_feature_groups(group_n + 1)
  o().groups[group_n].push(f)
  f.grouped = true
  f.group = group_n

fix_feature_overlap = () ->
  o().groups = []
  all_features = []
  for name, f_list of o().features_displayed
    for f in f_list
      if f.type == 'point'
        all_features.push(f)
  all_features.sort((a,b) -> a.at - b.at)

  last_start = undefined
  last_f = undefined

  for f in all_features
    #TODO: check for full circle first
    if last_start isnt undefined
      if angle_from_bp(f.at) - last_start < p('min_angular_dist')
        group_feature(f, o().groups.length - 1)
      else
        last_start = undefined
        group_feature(f, -1)
    else
      if last_f isnt undefined
        if angle_from_bp(f.at) - angle_from_bp(last_f.at) < p('min_angular_dist')
          last_start = angle_from_bp(last_f.at)
          new_group_index = o().groups.length
          group_feature(last_f, new_group_index)
          group_feature(f, new_group_index)
        else
          group_feature(f, -1)
    last_f = f




add_hidden_feature = (name, f) ->
  if o()['features_not_displayed'][name] == undefined
    o()['features_not_displayed'][name] = []
  
  o()['features_not_displayed'][name].push(f)

show_feature = (name) ->
  if o()['features_displayed'][name] == undefined
    o()['features_displayed'][name] = []

  if o()['features_not_displayed'][name] != undefined
    for f in o()['features_not_displayed'][name]
      o()['features_displayed'][name].push(f)
    o()['features_not_displayed'][name] = []

hide_feature = (name) ->
  if o()['features_displayed'][name] != undefined
    for f in o()['features_displayed'][name]
      o()['features_not_displayed'][name].push(f)
    delete o()['features_displayed'][name]

set_feature_from_field = (visible) ->
  name = $(p().dyn_enz_field).val()
  if visible
    show_feature(name)
  else
    hide_feature(name)
  redraw_map()

add_feature_from_field = () ->
  set_feature_from_field(true)

hide_feature_from_field = () -> 
  set_feature_from_field(false)



# Initialization functions

read_data = () -> 
  JSON.parse($(p('plas_map_div_id')).attr(p('data_attr')))

add_all_features = (f_collection) ->
  for name, f_list of f_collection
    for f in f_list
      add_hidden_feature(name, f)

show_initial_features = () ->
  always_on = p('initially_displayed_enzymes')

  for n, f of o().features_not_displayed
    if n in always_on
      show_feature(n, false)
    else if f.length == 1 and f[0].type == 'point'
      show_feature(n, false)  
    else if f.length > 0 and f[0].type == 'regional'
      show_feature(n, false)
  redraw_map()

initialize_features = () ->
  fs = read_data()
  o().data = fs
  o().features_displayed = {}
  o().features_not_displayed = {}
  p().pl_size = fs.pl_size
  add_all_features(fs['point_features'])
  add_all_features(fs['regional_features']) 
  show_initial_features()

initialize_buttons = () ->
  $(p('enz_remove_button')).click(hide_feature_from_field)
  $(p('enz_add_button')).click(add_feature_from_field)

do_map = () ->
  initialize_features()
  initialize_buttons()
  draw_all()

$("#show-map-button").on("click", do_map)


