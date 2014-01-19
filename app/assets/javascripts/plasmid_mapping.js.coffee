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
  map_height: 500,
  map_offset_x: 0,
  map_offset_y: -100,
  spacer_width: 10,

  min_angular_dist: Math.PI/16,
  max_label_in_place_char_count: 14,

  initially_displayed_enzymes: ['AscI', 'PacI'],
  initially_display_single_cutters: true,
}

parameters.map_radius = parameters.map_width/4
parameters.arc_width = parameters.map_width/96
parameters.inner_radius = parameters.map_radius - parameters.arc_width
parameters.outer_radius = parameters.map_radius + parameters.arc_width

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

feature_angle = (d,i) ->
  angle_from_bp(d.at)
  

# Drawing functions

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
  grp = o().svg.append("g")
  for f in o().point_features()
    grp.append("path")
      .attr("class", "line")
      .attr("d", d3.svg.line.radial()([[p('inner_radius'), feature_angle(f)], [p('outer_radius'), feature_angle(f)]]))
      .style("stroke", f.color)     

initialize_map = () ->
  o().svg = d3.select(p('plas_map_div_id')).append("div")
    .attr("class", "chart")
    .style("width", "#{p('map_width')}px")
    .style("height", "#{p('map_height')}px")
    .append("svg")
      .attr("width", p('map_width'))
      .attr("height", p('map_height'))
    .append("g")
      .attr("transform", "translate(#{p('map_width')/2 + p('map_offset_x')}, #{p('map_height')/2 + p('map_offset_y')})")

clear_map = () ->
  if d3.select(".chart")
    d3.select(".chart").remove()

draw_all = () ->
  initialize_map()
  draw_circle()
  draw_point_features()

redraw_map = () ->
  clear_map()
  draw_all()


# Add/remove functions

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
    o()['features_displayed'][name] = []

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
  p().pl_size = fs.pl_size
  add_all_features(fs['point_features'])
  add_all_features(fs['regional_features']) 
  show_initial_features()

initialize_buttons = () ->
  $(p('enz_remove_button')).click(hide_feature_from_field)
  $(p('enz_add_btn')).click(add_feature_from_field)

do_map = () ->
  initialize_features()
  draw_all()

$("#show-map-button").on("click", do_map)


