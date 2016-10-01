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

##
# This is a set of functions for displaying maps of plasmids.  The acual
# finding of features is accomplished by lib/plasmid_mapping.rb.
##

$ = require("jquery")
d3 = require("d3")

enzymes_to_show = [
    "AatII", "AccI", "Acc65I", "AclI", "AfeI", "AflII", "AflIII", "AgeI", "AhdI",
    "AleI", "AluI", "AlwNI", "AoxI", "ApaI", "ApaBI", "ApaLI", "ApoI", "AscI",
    "AseI", "Asi256I", "AsiSI", "AvaI", "AvaII", "AvrII", "BaeGI", "BamHI", "BanI",
    "BanII", "BclI", "BfaI", "BglI", "BglII", "BlpI", "BmtI", "BsaAI", "BsaBI",
    "BsaHI", "BsaJI", "BsaWI", "BsiEI", "BsiHKAI", "BsiWI", "BslI", "Bsp1286I",
    "BspEI", "BspHI", "BsrFI", "BsrGI", "BssHII", "BstAPI", "BstBI", "BstEII",
    "BstNI", "BstUI", "BstXI", "BstYI", "BstZ17I", "Bsu36I", "BtgI", "BthCI",
    "Cac8I", "ChaI", "ClaI", "CviAII", "CviKI", "CviQI", "DdeI", "DpnI", "DraI",
    "DraIII", "DrdI", "EaeI", "EagI", "EcoHI", "EcoNI", "EcoO109I", "EcoRI",
    "EcoRV", "Eco53kI", "EsaBC3I", "FatI", "FmuI", "Fnu4HI", "FseI", "FspI", "HaeI",
    "HaeII", "HaeIII", "HauII", "HhaI", "HinP1I", "HincII", "HindIII", "HinfI",
    "HpaI", "HpaII", "Hpy99I", "Hpy166II", "Hpy188I", "Hpy188III", "HpyCH4III",
    "HpyCH4IV", "HpyCH4V", "KasI", "KpnI", "LpnI", "MboI", "McaTI", "MfeI", "MluI",
    "MluCI", "MscI", "MseI", "MslI", "MspA1I", "MwoI", "NaeI", "NarI", "NciI",
    "NcoI", "NdeI", "NgoMIV", "NheI", "NlaIII", "NlaIV", "Nli3877I", "NotI", "NruI",
    "NsiI", "NspI", "PabI", "PacI", "PciI", "PflMI", "PluTI", "PmeI", "PmlI",
    "Ppu10I", "PpuMI", "PshAI", "PsiI", "Psp03I", "PspGI", "PspOMI", "PspXI",
    "PssI", "PstI", "PvuI", "PvuII", "RsaI", "RsrII", "SacI", "SacII", "SalI",
    "Sau96I", "SbfI", "ScaI", "SciI", "ScrFI", "SelI", "SexAI", "SfcI", "SfiI",
    "SfoI", "SgrAI", "SmaI", "SmlI", "SnaBI", "SpeI", "SphI", "SrfI", "Sse8647I",
    "SspI", "Sth302II", "StuI", "StyI", "StyD4I", "SwaI", "TaqI", "TfiI", "TseI",
    "Tsp45I", "TspRI", "Tth111I", "UnbI", "VpaK11AI", "XbaI", "XcmI", "XhoI",
    "XmaI", "XmnI", "ZraI",
]
# constants -- names, sizes, etc.
parameters = {
  plas_map_div_id: "#plasmid-map",
  data_attr: "data",
  dyn_enz_field: "#enzyme",
  enz_remove_button: "#hide_enzyme",
  enz_add_button: "#show_enzyme",
  group_index_attr: 'group-index',
  feature_text_attr: 'feature-text',

  # CSS classes
  css_selected: 'feature-selected',
  css_featureinfo: 'feature-info',
  css_point_feature: 'feature-point-feature',
  css_regional_feature: 'regional-feature',
  css_point_group: 'feature-point-group',
  css_feature_label: 'feature-label',
  css_plasmid_label: 'plasmid-label',

  map_width: 500,
  map_height: 400,
  map_offset_x: 0,
  map_offset_y: 35,
  info_offset_y: 0,
  spacer_width: 10,
  point_group_offset: 2,
  point_group_thickness: 4,

  min_angular_dist: Math.PI/32,
  angle_min_thickness: Math.PI/256,

  initially_displayed_enzymes: ['AscI', 'PacI'],
  initially_display_single_cutters: true,
}

parameters.map_radius = parameters.map_width/4
parameters.arc_width = parameters.map_width/96
parameters.inner_radius = parameters.map_radius - parameters.arc_width
parameters.outer_radius = parameters.map_radius + parameters.arc_width
parameters.display_height = parameters.map_height

# holder for the various features
objects = {
  features_displayed: {},
  features_not_displayed: {},
}

# function to get a parameter
# Args:
# name (optional) the property name to retrieve; if
#   not supplied returns the parameters object
p = (name=undefined) ->
  if name
    parameters[name]
  else
    parameters

# function to get the holder for the features
o = () ->
  objects

# retrieves the point features that should be displayed
# return: an array of feature objects
objects.point_features = () ->
  f_to_draw = o().features_displayed
  point_f_to_draw = []
  for k,v of f_to_draw
    if v[0].type == 'point'
      for vi in v
        point_f_to_draw.push(vi)
  return point_f_to_draw

# retrieves the regional features that should be displayed
# return: an array of feature objects
objects.regional_features = () ->
  f_to_draw = o().features_displayed
  reg_f_to_draw =[]
  for k,v of f_to_draw
    if v[0].type == 'regional'
      for vi in v
        reg_f_to_draw.push(vi)
  return reg_f_to_draw


## Functions to calculate various properties of features

# calculates the angle in radians around the circular plasmid, given a position
#   in base pairs
angle_from_bp = (bp) ->
  2*Math.PI*bp/p('pl_size')

# calculates the x-coordinate given r,theta polar coordinates, accounting for
#   the fact that angle 0 points straight up
x_from_polar = (r, theta) ->
  r*Math.cos(theta - Math.PI/2)

# calculates the y-coordinate given r,theta polar coordinates, accounting for
#   the fact that angle 0 points straight up
y_from_polar = (r, theta) ->
  r*Math.sin(theta - Math.PI/2)

# calculates the angle of a point feature
# Args:
#   d: the point feature (responds to .at)
#   i: ignored
# Return: the angle (radians)
feature_angle = (d,i) ->
  angle_from_bp(d.at)

# formats the text for a group of point features
# Args:
#   g: the group, an array of features
#   i: the group index (displayed as is; use 1-indexes)
# Return: a string containing the text for the features
text_for_group = (g, i) ->
  strrep = ""
  for f in g
    if (strrep.length > 0)
      strrep += ", "
    else
      strrep += "[#{i}]: "
    strrep += "#{f.text} (#{f.at})"
  strrep

# calculates an offset correction for the map that compensates for the size of
#   any displayed regional/group feature information
calculate_map_offset = () ->
  if $(".#{p('css_featureinfo')}").length > 0
    p().info_offset_y = -1*$(".#{p('css_featureinfo')}").outerHeight(true)
  else
    p().info_offset_y = 0

# updates the height of the svg element to the current display height parameter
update_svg_height = () ->
  $("svg").attr("height", p('display_height'))
  $(".chart").attr("style", "width: #{p('map_width')}px; height: #{p('display_height')}px;")

# updates the svg transformation that offsets the origin of the map so that the
#   map doesn't move when feature information is displayed
update_offset = () ->
  $("#offset-group").attr("transform", "translate(#{p('map_width')/2 + p('map_offset_x')}, #{p('map_height')/2 + p('map_offset_y') + p('info_offset_y')})")
  p().display_height = p().map_height + p().info_offset_y
  update_svg_height()

## Functions for dealing with mouseover interaction with feature groups

# Remove active feature highlighting
reset_point_group_highlight = () ->
  $(".#{p('css_selected')}").removeClass(p('css_selected'))

# Reset the information box
reset_feature_info = () ->
  p().info_offset_y = 0
  update_offset()
  reset_point_group_highlight()
  $(".#{p('css_featureinfo')}").remove()

# Highlights the selected feature
highlight_expanded_feature = (f_element) ->
  reset_point_group_highlight()
  $(f_element).addClass(p('css_selected'))

# Adds an information box for the feature information and updates the map
# position to compensate for the box
set_up_feature_information_box = (text) ->
  $(".#{p('css_featureinfo')}").remove()
  info_el = document.createElement("div")
  info_el.className = "feature-info"
  $(p('plas_map_div_id')).prepend(info_el)
  $(".#{p('css_featureinfo')}").append('<button type="button" class="close"><i class="material-icons">highlight_off</i></button>')
  $(".#{p('css_featureinfo')}").append("<div>#{text}</div>")
  $('button.close').click(reset_feature_info)
  calculate_map_offset()
  update_offset()

# Expands a feature by highlighting it and additing an info box
do_feature_expand = (f_element, text) ->
  highlight_expanded_feature(f_element)
  set_up_feature_information_box(text)

# Mouseover event handler for expanding a group of point features
do_expand_point_group = (event) ->
  f_element = event.target
  gr_index = parseInt($(f_element).attr(p('group_index_attr')))
  g = o().groups[gr_index]
  text = text_for_group(g, gr_index + 1)
  do_feature_expand(f_element, text)

# Mouseover event handler for expanding a regional feature
regional_feature_information = (event) ->
  f_element = event.target
  text = f_element.getAttribute(p('feature_text_attr'))
  do_feature_expand(f_element, text)

## Drawing functions

# Determines whether to anchor a label at the left or right based on the map
# angle
calculate_text_anchor_point = (angle) ->
  if angle < Math.PI
    return "start"
  else
    return "end"

# Draws the circle representing the plasmid
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

# Draw all of the point features to be displayed
draw_point_features = () ->
  #TODO: use data
  grp = o().svg.append("g")
  for f in o().point_features()
    grp.append("path")
      .attr("class", "line #{p('css_point_feature')}")
      .attr("d", d3.svg.line.radial()([[p('inner_radius'), feature_angle(f)], [p('outer_radius'), feature_angle(f)]]))

# Draw all of the regional features to be displayed
draw_regional_features = () ->
  grp = o().svg.append("g")
  for f in o().regional_features()
    grp.append("path")
      .attr("class", "arc #{p('css_regional_feature')} feature-#{f.feature_class}")
      .attr(p('feature_text_attr'), "#{f.text} (#{f.start} - #{f.start + f.length - 1})")
      .attr("d", d3.svg.arc()
                  .outerRadius(p('outer_radius'))
                  .innerRadius(p('inner_radius'))
                  .startAngle(angle_from_bp(f.start))
                  .endAngle(angle_from_bp(f.start + f.length)))
  $(".#{p('css_regional_feature')}").mouseover(regional_feature_information)

# Draw all of the groups of overlapping point features
draw_point_groups = () ->
  #TODO: use data
  grp = o().svg.append("g")
  for g, i in o().groups
    grp.append("path")
      .attr("class", "arc #{p('css_point_group')}")
      .attr("group-index", i)
      .attr("d", d3.svg.arc()
                  .outerRadius(p('outer_radius') + p('point_group_offset') + p('point_group_thickness'))
                  .innerRadius(p('outer_radius') + p('point_group_offset'))
                  .startAngle(angle_from_bp(g[0].at) - p('angle_min_thickness'))
                  .endAngle(angle_from_bp(g[g.length - 1].at) + p('angle_min_thickness')))
  $(".#{p('css_point_group')}").mouseover(do_expand_point_group)

# Draw a label for one of the features
# Args:
#   grp: the svg group ("g") element to contain the labels
#   angle: the angle of the feature
#   text: the text to use for the label
#   radial_offset (default 3): the offset in px from the outer_radius parameter
#   text_rotation (default true): whether to rotate the text slightly for
#     better packing near the poles
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
    .attr("class", p('css_feature_label'))
    .attr("x", x)
    .attr("y", y)
    .attr("text-anchor", calculate_text_anchor_point(angle))
    .text(text)
    .attr("transform", "rotate(#{rotation_angle}, #{x}, #{y})")

# Draw all the point feature labels
draw_point_labels = () ->
  grp = o().svg.append("g")
  for f in o().point_features()
    if f.grouped
      continue
    angle = angle_from_bp(f.at)
    text = "#{f.text} (#{f.at})"
    draw_single_feature_label(grp, angle, text)

# Draw all the point feature group labels
draw_group_labels = () ->
  grp = o().svg.append("g")
  for g, i in o().groups
    angle = (angle_from_bp(g[0].at) + angle_from_bp(g[g.length - 1].at))/2
    text = "#{i+1}"
    draw_single_feature_label(grp, angle, text, 10, false)

# Draw the plasmid label
draw_plasmid_label = () ->
  grp = o().svg.append("g")
  grp.append("text")
    .attr("class", "#{p('css_plasmid_label')}")
    .text("#{p('pl_name')} (#{p('pl_size')}bp)")
    .attr("text-anchor", "middle")

# Initialize all the elements necessary to draw the map
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

# Clear the map
clear_map = () ->
  $('.chart').remove()

# Draw all of the elements of the map
draw_all = () ->
  fix_feature_overlap()
  calculate_map_offset()
  update_offset()
  initialize_map()
  update_offset()
  draw_circle()
  draw_regional_features()
  draw_point_features()
  draw_point_groups()
  draw_point_labels()
  draw_group_labels()
  draw_plasmid_label()

# Redraw the map if it has been displayed already
redraw_map = () ->
  clear_map()
  draw_all()

## Feature handling functions

# Helper to ensure that there are at least as many point feature groups as
# necessary
ensure_n_feature_groups = (n) ->
  while o().groups.length < n
    o().groups.push([])

# Adds a feature to the specified group.
# Args:
#   f: the feature to add
#   group_n: the 0-indexed group number; pass a negative number to indicate
#     not grouped
group_feature = (f, group_n) ->
  if group_n < 0
    f.grouped = false
    return
  ensure_n_feature_groups(group_n + 1)
  o().groups[group_n].push(f)
  f.grouped = true
  f.group = group_n

# Fixes overlapping point features by adding closely spaced features to a group
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

# Add a feature to the map, but don't display it yet
add_hidden_feature = (name, f) ->
  if o()['features_not_displayed'][name] == undefined
    o()['features_not_displayed'][name] = []

  o()['features_not_displayed'][name].push(f)

# Shows a previously hidden feature
show_feature = (name) ->
  if o()['features_displayed'][name] == undefined
    o()['features_displayed'][name] = []

  if o()['features_not_displayed'][name] != undefined
    for f in o()['features_not_displayed'][name]
      o()['features_displayed'][name].push(f)
    o()['features_not_displayed'][name] = []

# Hides a previously shown feature
hide_feature = (name) ->
  if o()['features_displayed'][name] != undefined
    for f in o()['features_displayed'][name]
      o()['features_not_displayed'][name].push(f)
    delete o()['features_displayed'][name]

# Set the visibility of a feature grabbed from the feature field according to # whether the show or hide button was pressed.
set_feature_from_field = (visible) ->
  name = $(p().dyn_enz_field).val()
  if visible
    show_feature(name)
  else
    hide_feature(name)
  redraw_map()

# Show a feature named by the user in the feature field
add_feature_from_field = () ->
  set_feature_from_field(true)

# Hide a feature named by the user in the feature field
hide_feature_from_field = () ->
  set_feature_from_field(false)

## Initialization functions

# Read, parse, and return the json plasmid map data from the plasmid map page
# element
read_data = () ->
  data = JSON.parse($(p('plas_map_div_id')).attr(p('data_attr')))
  data


# Add all features in a collection to the map (initially hidden)
# Input should be a mapping {name: [list of features with that name]}
add_all_features = (f_collection) ->
  for name, f_list of f_collection
    for f in f_list
      add_hidden_feature(name, f)

# Show the features that should be initially displayed
show_initial_features = () ->
  always_on = p('initially_displayed_enzymes')

  for n, f of o().features_not_displayed
    if n in always_on
      show_feature(n, false)
    else if f.length > 0 and f[0].type == 'point' and enzymes_to_show.indexOf(f[0].text) < 0
      hide_feature(n)
    else if f.length == 1 and f[0].type == 'point'
      show_feature(n, false)
    else if f.length > 0 and f[0].type == 'regional'
      show_feature(n, false)

# Do all of the feature setup, including reading from the page
initialize_features = () ->
  fs = read_data()
  o().data = fs
  o().features_displayed = {}
  o().features_not_displayed = {}
  p().pl_size = fs.pl_size
  p().pl_name = fs.pl_name
  add_all_features(fs['point_features'])
  add_all_features(fs['regional_features'])
  show_initial_features()

# Set up the show/hide button click handlers
initialize_buttons = () ->
  $(p('enz_remove_button')).click(hide_feature_from_field)
  $(p('enz_add_button')).click(add_feature_from_field)

# Do everything required to make the map
do_map = () ->
  clear_map()
  initialize_features()
  initialize_buttons()
  draw_all()

module.exports = do_map
