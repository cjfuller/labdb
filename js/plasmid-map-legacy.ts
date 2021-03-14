/*
 * decaffeinate suggestions:
 * DS101: Remove unnecessary use of Array.from
 * DS102: Remove unnecessary code created because of implicit returns
 * DS205: Consider reworking code to avoid use of IIFEs
 * DS207: Consider shorter variations of null checks
 * Full docs: https://github.com/decaffeinate/decaffeinate/blob/master/docs/suggestions.md
 */
// TODO(colin): also make this autoconversion generally more TS-friendly.
//#
// This is a set of functions for displaying maps of plasmids.  The acual
// finding of features is accomplished by lib/plasmid_mapping.rb.
//#

import $ from "jquery";

import d3 from "d3";

const enzymes_to_show = [
  "AatII",
  "AccI",
  "Acc65I",
  "AclI",
  "AfeI",
  "AflII",
  "AflIII",
  "AgeI",
  "AhdI",
  "AleI",
  "AluI",
  "AlwNI",
  "AoxI",
  "ApaI",
  "ApaBI",
  "ApaLI",
  "ApoI",
  "AscI",
  "AseI",
  "Asi256I",
  "AsiSI",
  "AvaI",
  "AvaII",
  "AvrII",
  "BaeGI",
  "BamHI",
  "BanI",
  "BanII",
  "BclI",
  "BfaI",
  "BglI",
  "BglII",
  "BlpI",
  "BmtI",
  "BsaAI",
  "BsaBI",
  "BsaHI",
  "BsaJI",
  "BsaWI",
  "BsiEI",
  "BsiHKAI",
  "BsiWI",
  "BslI",
  "Bsp1286I",
  "BspEI",
  "BspHI",
  "BsrFI",
  "BsrGI",
  "BssHII",
  "BstAPI",
  "BstBI",
  "BstEII",
  "BstNI",
  "BstUI",
  "BstXI",
  "BstYI",
  "BstZ17I",
  "Bsu36I",
  "BtgI",
  "BthCI",
  "Cac8I",
  "ChaI",
  "ClaI",
  "CviAII",
  "CviKI",
  "CviQI",
  "DdeI",
  "DpnI",
  "DraI",
  "DraIII",
  "DrdI",
  "EaeI",
  "EagI",
  "EcoHI",
  "EcoNI",
  "EcoO109I",
  "EcoRI",
  "EcoRV",
  "Eco53kI",
  "EsaBC3I",
  "FatI",
  "FmuI",
  "Fnu4HI",
  "FseI",
  "FspI",
  "HaeI",
  "HaeII",
  "HaeIII",
  "HauII",
  "HhaI",
  "HinP1I",
  "HincII",
  "HindIII",
  "HinfI",
  "HpaI",
  "HpaII",
  "Hpy99I",
  "Hpy166II",
  "Hpy188I",
  "Hpy188III",
  "HpyCH4III",
  "HpyCH4IV",
  "HpyCH4V",
  "KasI",
  "KpnI",
  "LpnI",
  "MboI",
  "McaTI",
  "MfeI",
  "MluI",
  "MluCI",
  "MscI",
  "MseI",
  "MslI",
  "MspA1I",
  "MwoI",
  "NaeI",
  "NarI",
  "NciI",
  "NcoI",
  "NdeI",
  "NgoMIV",
  "NheI",
  "NlaIII",
  "NlaIV",
  "Nli3877I",
  "NotI",
  "NruI",
  "NsiI",
  "NspI",
  "PabI",
  "PacI",
  "PciI",
  "PflMI",
  "PluTI",
  "PmeI",
  "PmlI",
  "Ppu10I",
  "PpuMI",
  "PshAI",
  "PsiI",
  "Psp03I",
  "PspGI",
  "PspOMI",
  "PspXI",
  "PssI",
  "PstI",
  "PvuI",
  "PvuII",
  "RsaI",
  "RsrII",
  "SacI",
  "SacII",
  "SalI",
  "Sau96I",
  "SbfI",
  "ScaI",
  "SciI",
  "ScrFI",
  "SelI",
  "SexAI",
  "SfcI",
  "SfiI",
  "SfoI",
  "SgrAI",
  "SmaI",
  "SmlI",
  "SnaBI",
  "SpeI",
  "SphI",
  "SrfI",
  "Sse8647I",
  "SspI",
  "Sth302II",
  "StuI",
  "StyI",
  "StyD4I",
  "SwaI",
  "TaqI",
  "TfiI",
  "TseI",
  "Tsp45I",
  "TspRI",
  "Tth111I",
  "UnbI",
  "VpaK11AI",
  "XbaI",
  "XcmI",
  "XhoI",
  "XmaI",
  "XmnI",
  "ZraI",
];
// constants -- names, sizes, etc.
const parameters: any = {
  plas_map_div_id: "#plasmid-map",
  data_attr: "data-mapinfo",
  dyn_enz_field: "#enzyme",
  enz_remove_button: "#hide_enzyme",
  enz_add_button: "#show_enzyme",
  group_index_attr: "group-index",
  feature_text_attr: "feature-text",

  // CSS classes
  css_selected: "feature-selected",
  css_featureinfo: "feature-info",
  css_point_feature: "feature-point-feature",
  css_regional_feature: "regional-feature",
  css_point_group: "feature-point-group",
  css_feature_label: "feature-label",
  css_plasmid_label: "plasmid-label",

  map_width: 500,
  map_height: 400,
  map_offset_x: 0,
  map_offset_y: 35,
  info_offset_y: 0,
  spacer_width: 10,
  point_group_offset: 2,
  point_group_thickness: 4,

  min_angular_dist: Math.PI / 32,
  angle_min_thickness: Math.PI / 256,

  initially_displayed_enzymes: ["AscI", "PacI"],
  initially_display_single_cutters: true,
};

parameters.map_radius = parameters.map_width / 4;
parameters.arc_width = parameters.map_width / 96;
parameters.inner_radius = parameters.map_radius - parameters.arc_width;
parameters.outer_radius = parameters.map_radius + parameters.arc_width;
parameters.display_height = parameters.map_height;

// holder for the various features
const objects: any = {
  features_displayed: {},
  features_not_displayed: {},
};

// function to get a parameter
// Args:
// name (optional) the property name to retrieve; if
//   not supplied returns the parameters object
const p = function (name?: any) {
  if (name == null) {
    name = undefined;
  }
  if (name) {
    return parameters[name];
  } else {
    return parameters;
  }
};

// function to get the holder for the features
const o = () => objects;

// retrieves the point features that should be displayed
// return: an array of feature objects
objects.point_features = function () {
  const f_to_draw = o().features_displayed;
  const point_f_to_draw = [];
  for (let k in f_to_draw) {
    const v = f_to_draw[k];
    if (v[0].type === "point") {
      for (let vi of Array.from(v)) {
        point_f_to_draw.push(vi);
      }
    }
  }
  return point_f_to_draw;
};

// retrieves the regional features that should be displayed
// return: an array of feature objects
objects.regional_features = function () {
  const f_to_draw = o().features_displayed;
  const reg_f_to_draw = [];
  for (let k in f_to_draw) {
    const v = f_to_draw[k];
    if (v[0].type === "regional") {
      for (let vi of Array.from(v)) {
        reg_f_to_draw.push(vi);
      }
    }
  }
  return reg_f_to_draw;
};

//# Functions to calculate various properties of features

// calculates the angle in radians around the circular plasmid, given a position
//   in base pairs
const angle_from_bp = (bp: number) => (2 * Math.PI * bp) / p("pl_size");

// calculates the x-coordinate given r,theta polar coordinates, accounting for
//   the fact that angle 0 points straight up
const x_from_polar = (r: number, theta: number) =>
  r * Math.cos(theta - Math.PI / 2);

// calculates the y-coordinate given r,theta polar coordinates, accounting for
//   the fact that angle 0 points straight up
const y_from_polar = (r: number, theta: number) =>
  r * Math.sin(theta - Math.PI / 2);

// calculates the angle of a point feature
// Args:
//   d: the point feature (responds to .at)
//   i: ignored
// Return: the angle (radians)
const feature_angle = (d: any, i?: undefined) => angle_from_bp(d.at);

// formats the text for a group of point features
// Args:
//   g: the group, an array of features
//   i: the group index (displayed as is; use 1-indexes)
// Return: a string containing the text for the features
const text_for_group = function (g: Iterable<any> | ArrayLike<any>, i: number) {
  let strrep = "";
  for (let f of Array.from(g)) {
    if (strrep.length > 0) {
      strrep += ", ";
    } else {
      strrep += `[${i}]: `;
    }
    strrep += `${f.text} (${f.at})`;
  }
  return strrep;
};

// calculates an offset correction for the map that compensates for the size of
//   any displayed regional/group feature information
const calculate_map_offset = function () {
  if ($(`.${p("css_featureinfo")}`).length > 0) {
    return (p().info_offset_y =
      -1 * ($(`.${p("css_featureinfo")}`)?.outerHeight(true) ?? 0));
  } else {
    return (p().info_offset_y = 0);
  }
};

// updates the height of the svg element to the current display height parameter
const update_svg_height = function () {
  $("svg").attr("height", p("display_height"));
  return $(".chart").attr(
    "style",
    `width: ${p("map_width")}px; height: ${p("display_height")}px;`
  );
};

// updates the svg transformation that offsets the origin of the map so that the
//   map doesn't move when feature information is displayed
const update_offset = function () {
  $("#offset-group").attr(
    "transform",
    `translate(${p("map_width") / 2 + p("map_offset_x")}, ${
      p("map_height") / 2 + p("map_offset_y") + p("info_offset_y")
    })`
  );
  p().display_height = p().map_height + p().info_offset_y;
  return update_svg_height();
};

//# Functions for dealing with mouseover interaction with feature groups

// Remove active feature highlighting
const reset_point_group_highlight = () =>
  $(`.${p("css_selected")}`).removeClass(p("css_selected"));

// Reset the information box
const reset_feature_info = function () {
  p().info_offset_y = 0;
  update_offset();
  reset_point_group_highlight();
  return $(`.${p("css_featureinfo")}`).remove();
};

// Highlights the selected feature
const highlight_expanded_feature = function (f_element: any) {
  reset_point_group_highlight();
  return $(f_element).addClass(p("css_selected"));
};

// Adds an information box for the feature information and updates the map
// position to compensate for the box
const set_up_feature_information_box = function (text: any) {
  $(`.${p("css_featureinfo")}`).remove();
  const info_el = document.createElement("div");
  info_el.className = "feature-info";
  $(p("plas_map_div_id")).prepend(info_el);
  $(`.${p("css_featureinfo")}`).append(
    '<button type="button" class="close"><i class="material-icons">highlight_off</i></button>'
  );
  $(`.${p("css_featureinfo")}`).append(`<div>${text}</div>`);
  $("button.close").click(reset_feature_info);
  calculate_map_offset();
  return update_offset();
};

// Expands a feature by highlighting it and additing an info box
const do_feature_expand = function (f_element: any, text: string) {
  highlight_expanded_feature(f_element);
  return set_up_feature_information_box(text);
};

// Mouseover event handler for expanding a group of point features
const do_expand_point_group = function (event: { target: any }) {
  const f_element = event.target;
  const gr_index = parseInt($(f_element).attr(p("group_index_attr")) as any);
  const g = o().groups[gr_index];
  const text = text_for_group(g, gr_index + 1);
  return do_feature_expand(f_element, text);
};

// Mouseover event handler for expanding a regional feature
const regional_feature_information = function (event: { target: any }) {
  const f_element = event.target;
  const text = f_element.getAttribute(p("feature_text_attr"));
  return do_feature_expand(f_element, text);
};

//# Drawing functions

// Determines whether to anchor a label at the left or right based on the map
// angle
const calculate_text_anchor_point = function (angle: number) {
  if (angle < Math.PI) {
    return "start";
  } else {
    return "end";
  }
};

// Draws the circle representing the plasmid
const draw_circle = () =>
  o()
    .svg.append("g")
    .selectAll("circle")
    .data([0])
    .enter()
    .append("circle")
    .attr("cx", 0)
    .attr("cy", 0)
    .attr("r", p("map_radius"))
    .style("fill", "none")
    .style("stroke", "#000");

// Draw all of the point features to be displayed
const draw_point_features = function () {
  //TODO: use data
  const grp = o().svg.append("g");
  return Array.from(o().point_features()).map((f) =>
    grp
      .append("path")
      .attr("class", `line ${p("css_point_feature")}`)
      .attr(
        "d",
        (d3.svg as any).line.radial()([
          [p("inner_radius"), feature_angle(f)],
          [p("outer_radius"), feature_angle(f)],
        ])
      )
  );
};

// Draw all of the regional features to be displayed
const draw_regional_features = function () {
  const grp = o().svg.append("g");
  o()
    .regional_features()
    .foreach((f: any) => {
      grp
        .append("path")
        .attr(
          "class",
          `arc ${p("css_regional_feature")} feature-${f.feature_class}`
        )
        .attr(
          p("feature_text_attr"),
          `${f.text} (${f.start} - ${f.start + f.length - 1})`
        )
        .attr(
          "d",
          (d3.svg as any)
            .arc()
            .outerRadius(p("outer_radius"))
            .innerRadius(p("inner_radius"))
            .startAngle(angle_from_bp(f.start))
            .endAngle(angle_from_bp(f.start + f.length))
        );
    });
  return $(`.${p("css_regional_feature")}`).mouseover(
    regional_feature_information
  );
};

// Draw all of the groups of overlapping point features
const draw_point_groups = function () {
  //TODO: use data
  const grp = o().svg.append("g");
  const iterable = o().groups;
  for (let i = 0; i < iterable.length; i++) {
    const g = iterable[i];
    grp
      .append("path")
      .attr("class", `arc ${p("css_point_group")}`)
      .attr("group-index", i)
      .attr(
        "d",
        (d3.svg as any)
          .arc()
          .outerRadius(
            p("outer_radius") +
              p("point_group_offset") +
              p("point_group_thickness")
          )
          .innerRadius(p("outer_radius") + p("point_group_offset"))
          .startAngle(angle_from_bp(g[0].at) - p("angle_min_thickness"))
          .endAngle(
            angle_from_bp(g[g.length - 1].at) + p("angle_min_thickness")
          )
      );
  }
  return $(`.${p("css_point_group")}`).mouseover(do_expand_point_group);
};

// Draw a label for one of the features
// Args:
//   grp: the svg group ("g") element to contain the labels
//   angle: the angle of the feature
//   text: the text to use for the label
//   radial_offset (default 3): the offset in px from the outer_radius parameter
//   text_rotation (default true): whether to rotate the text slightly for
//     better packing near the poles
const draw_single_feature_label = function (
  grp: any,
  angle: number,
  text: string,
  radial_offset?: number | null | undefined,
  text_rotation?: boolean | null | undefined
) {
  if (radial_offset == null) {
    radial_offset = 3;
  }
  if (text_rotation == null) {
    text_rotation = true;
  }
  let symm_angle = angle;
  if (angle > Math.PI) {
    symm_angle = Math.abs(angle - Math.PI * 2);
  }
  let rotation_angle = ((0.25 * (symm_angle - Math.PI / 2)) / Math.PI) * 180;
  if (!text_rotation) {
    rotation_angle = 0;
  }
  if (angle > Math.PI) {
    rotation_angle *= -1;
  }
  const x = x_from_polar(p("outer_radius") + radial_offset, angle);
  const y =
    y_from_polar(p("outer_radius") + radial_offset, angle) +
    Math.sin(symm_angle / 2) * 8;
  return grp
    .append("text")
    .attr("class", p("css_feature_label"))
    .attr("x", x)
    .attr("y", y)
    .attr("text-anchor", calculate_text_anchor_point(angle))
    .text(text)
    .attr("transform", `rotate(${rotation_angle}, ${x}, ${y})`);
};

// Draw all the point feature labels
const draw_point_labels = function () {
  const grp = o().svg.append("g");
  return (() => {
    const result: any[] = [];
    o().point_features.foreach((f: any) => {
      if (f.grouped) {
        return;
      }
      const angle = angle_from_bp(f.at);
      const text = `${f.text} (${f.at})`;
      result.push(draw_single_feature_label(grp, angle, text));
    });
    return result;
  })();
};

// Draw all the point feature group labels
const draw_group_labels = function () {
  const grp = o().svg.append("g");
  return (() => {
    const result = [];
    const iterable = o().groups;
    for (let i = 0; i < iterable.length; i++) {
      const g = iterable[i];
      const angle =
        (angle_from_bp(g[0].at) + angle_from_bp(g[g.length - 1].at)) / 2;
      const text = `${i + 1}`;
      result.push(draw_single_feature_label(grp, angle, text, 10, false));
    }
    return result;
  })();
};

// Draw the plasmid label
const draw_plasmid_label = function () {
  const grp = o().svg.append("g");
  return grp
    .append("text")
    .attr("class", `${p("css_plasmid_label")}`)
    .text(`${p("pl_name")} (${p("pl_size")}bp)`)
    .attr("text-anchor", "middle");
};

// Initialize all the elements necessary to draw the map
const initialize_map = function () {
  o().svg = d3
    .select(p("plas_map_div_id"))
    .append("div")
    .attr("class", "chart")
    .style("width", `${p("map_width")}px`)
    .style("height", `${p("display_height")}px`)
    .append("svg")
    .attr("width", p("map_width"))
    .attr("height", p("display_height"))
    .append("g")
    .attr("id", "offset-group")
    .attr(
      "transform",
      `translate(${p("map_width") / 2 + p("map_offset_x")}, ${
        p("map_height") / 2 + p("map_offset_y") + p("info_offset_y")
      })`
    );
  return update_offset();
};

// Clear the map
const clear_map = () => $(".chart").remove();

// Draw all of the elements of the map
const draw_all = function () {
  fix_feature_overlap();
  calculate_map_offset();
  update_offset();
  initialize_map();
  update_offset();
  draw_circle();
  draw_regional_features();
  draw_point_features();
  draw_point_groups();
  draw_point_labels();
  draw_group_labels();
  return draw_plasmid_label();
};

// Redraw the map if it has been displayed already
const redraw_map = function () {
  clear_map();
  return draw_all();
};

//# Feature handling functions

// Helper to ensure that there are at least as many point feature groups as
// necessary
const ensure_n_feature_groups = (n: number) =>
  (() => {
    const result = [];
    while (o().groups.length < n) {
      result.push(o().groups.push([]));
    }
    return result;
  })();

// Adds a feature to the specified group.
// Args:
//   f: the feature to add
//   group_n: the 0-indexed group number; pass a negative number to indicate
//     not grouped
const group_feature = function (f: any, group_n: number) {
  if (group_n < 0) {
    f.grouped = false;
    return;
  }
  ensure_n_feature_groups(group_n + 1);
  o().groups[group_n].push(f);
  f.grouped = true;
  return (f.group = group_n);
};

// Fixes overlapping point features by adding closely spaced features to a group
var fix_feature_overlap = function () {
  let f: any;
  o().groups = [];
  const all_features = [];
  const object = o().features_displayed;
  for (let name in object) {
    const f_list = object[name];
    for (f of Array.from(f_list)) {
      if (f.type === "point") {
        all_features.push(f);
      }
    }
  }
  all_features.sort((a, b) => a.at - b.at);

  let last_start = undefined;
  let last_f = undefined;

  return (() => {
    const result = [];
    for (f of Array.from(all_features)) {
      //TODO: check for full circle first
      if (last_start !== undefined) {
        if (angle_from_bp(f.at) - last_start < p("min_angular_dist")) {
          group_feature(f, o().groups.length - 1);
        } else {
          last_start = undefined;
          group_feature(f, -1);
        }
      } else {
        if (last_f !== undefined) {
          if (
            angle_from_bp(f.at) - angle_from_bp(last_f.at) <
            p("min_angular_dist")
          ) {
            last_start = angle_from_bp(last_f.at);
            const new_group_index = o().groups.length;
            group_feature(last_f, new_group_index);
            group_feature(f, new_group_index);
          } else {
            group_feature(f, -1);
          }
        }
      }
      result.push((last_f = f));
    }
    return result;
  })();
};

// Add a feature to the map, but don't display it yet
const add_hidden_feature = function (name: string, f: unknown) {
  if (o()["features_not_displayed"][name] === undefined) {
    o()["features_not_displayed"][name] = [];
  }

  return o()["features_not_displayed"][name].push(f);
};

// Shows a previously hidden feature
const show_feature = function (name: string) {
  if (o()["features_displayed"][name] === undefined) {
    o()["features_displayed"][name] = [];
  }

  if (o()["features_not_displayed"][name] !== undefined) {
    for (let f of Array.from(o()["features_not_displayed"][name])) {
      o()["features_displayed"][name].push(f);
    }
    return (o()["features_not_displayed"][name] = []);
  }
};

// Hides a previously shown feature
const hide_feature = function (name: string) {
  if (o()["features_displayed"][name] !== undefined) {
    for (let f of Array.from(o()["features_displayed"][name])) {
      o()["features_not_displayed"][name].push(f);
    }
    return delete o()["features_displayed"][name];
  }
};

// Set the visibility of a feature grabbed from the feature field according to # whether the show or hide button was pressed.
const set_feature_from_field = function (visible: boolean) {
  const name: any = $(p().dyn_enz_field).val();
  if (visible) {
    show_feature(name);
  } else {
    hide_feature(name);
  }
  return redraw_map();
};

// Show a feature named by the user in the feature field
const add_feature_from_field = () => set_feature_from_field(true);

// Hide a feature named by the user in the feature field
const hide_feature_from_field = () => set_feature_from_field(false);

//# Initialization functions

// Read, parse, and return the json plasmid map data from the plasmid map page
// element
const read_data = function () {
  const data = JSON.parse($(p("plas_map_div_id")).attr(p("data_attr")) as any);
  return data;
};

// Add all features in a collection to the map (initially hidden)
// Input should be a mapping {name: [list of features with that name]}
const add_all_features = (f_collection: { [x: string]: any }) =>
  (() => {
    const result = [];
    for (var name in f_collection) {
      const f_list = f_collection[name];
      result.push(Array.from(f_list).map((f) => add_hidden_feature(name, f)));
    }
    return result;
  })();

// Show the features that should be initially displayed
const show_initial_features = function () {
  const always_on = p("initially_displayed_enzymes");

  return (() => {
    const result = [];
    const object = o().features_not_displayed;
    for (let n in object) {
      const f = object[n];
      if (Array.from(always_on).includes(n)) {
        result.push(show_feature(n));
      } else if (
        f.length > 0 &&
        f[0].type === "point" &&
        enzymes_to_show.indexOf(f[0].text) < 0
      ) {
        result.push(hide_feature(n));
      } else if (f.length === 1 && f[0].type === "point") {
        result.push(show_feature(n));
      } else if (f.length > 0 && f[0].type === "regional") {
        result.push(show_feature(n));
      } else {
        result.push(undefined);
      }
    }
    return result;
  })();
};

// Do all of the feature setup, including reading from the page
const initialize_features = function () {
  const fs = read_data();
  o().data = fs;
  o().features_displayed = {};
  o().features_not_displayed = {};
  p().pl_size = fs.pl_size;
  p().pl_name = fs.pl_name;
  add_all_features(fs["point_features"]);
  add_all_features(fs["regional_features"]);
  return show_initial_features();
};

// Set up the show/hide button click handlers
const initialize_buttons = function () {
  $(p("enz_remove_button")).click(hide_feature_from_field);
  return $(p("enz_add_button")).click(add_feature_from_field);
};

// Do everything required to make the map
const do_map = function () {
  clear_map();
  initialize_features();
  initialize_buttons();
  return draw_all();
};

export default do_map;
