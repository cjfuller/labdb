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

const enzymes_to_show = new Set([
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
]);

type BaseFeature = {
  feature_class: "restriction" | "affinity tag" | "protease";
  text: string;
  grouped?: boolean;
  group?: number;
};

type PointFeature = BaseFeature & {
  type: "point";
  at: number;
  count: number;
};

type RegionalFeature = BaseFeature & {
  type: "regional";
  length: number;
  start: number;
};

type Feature = PointFeature | RegionalFeature;

const mapWidth = 500;
const mapHeight = 400;
const mapRadius = mapWidth / 4;
const arcWidth = mapWidth / 96;

// constants -- names, sizes, etc.
const parameters = {
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

  map_width: mapWidth,
  map_height: mapHeight,
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

  map_radius: mapRadius,
  arc_width: arcWidth,
  inner_radius: mapRadius - arcWidth,
  outer_radius: mapRadius + arcWidth,
  display_height: mapHeight,
};

const pointFeatures = (objects: Objects): PointFeature[] =>
  Object.entries(objects.features)
    .filter(
      (e) =>
        e[1][0].type === "point" && objects.featureNamesToDisplay.includes(e[0])
    )
    .flatMap((e) => e[1]) as PointFeature[];

const regionalFeatures = (objects: Objects): RegionalFeature[] =>
  Object.entries(objects.features)
    .filter(
      (e) =>
        e[1][0].type === "regional" &&
        objects.featureNamesToDisplay.includes(e[0])
    )
    .flatMap((e) => e[1]) as RegionalFeature[];

//# Functions to calculate various properties of features

// calculates the angle in radians around the circular plasmid, given a position
//   in base pairs
const angle_from_bp = (o: Objects, bp: number) =>
  (2 * Math.PI * bp) / o.pl_size;

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
const feature_angle = (o: Objects, d: any, i?: undefined) =>
  angle_from_bp(o, d.at);

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
  if ($(`.${parameters.css_featureinfo}`).length > 0) {
    return (parameters.info_offset_y =
      -1 * ($(`.${parameters.css_featureinfo}`)?.outerHeight(true) ?? 0));
  } else {
    return (parameters.info_offset_y = 0);
  }
};

// updates the height of the svg element to the current display height parameter
const update_svg_height = function () {
  $("svg").attr("height", parameters.display_height);
  return $(".chart").attr(
    "style",
    `width: ${parameters.map_width}px; height: ${parameters.display_height}px;`
  );
};

// updates the svg transformation that offsets the origin of the map so that the
//   map doesn't move when feature information is displayed
const update_offset = function () {
  $("#offset-group").attr(
    "transform",
    `translate(${parameters.map_width / 2 + parameters.map_offset_x}, ${
      parameters.map_height / 2 +
      parameters.map_offset_y +
      parameters.info_offset_y
    })`
  );
  parameters.display_height = parameters.map_height + parameters.info_offset_y;
  return update_svg_height();
};

//# Functions for dealing with mouseover interaction with feature groups

// Remove active feature highlighting
const reset_point_group_highlight = () =>
  $(`.${parameters.css_selected}`).removeClass(parameters.css_selected);

// Reset the information box
const reset_feature_info = function () {
  parameters.info_offset_y = 0;
  update_offset();
  reset_point_group_highlight();
  return $(`.${parameters.css_featureinfo}`).remove();
};

// Highlights the selected feature
const highlight_expanded_feature = function (f_element: any) {
  reset_point_group_highlight();
  return $(f_element).addClass(parameters.css_selected);
};

// Adds an information box for the feature information and updates the map
// position to compensate for the box
const set_up_feature_information_box = function (text: any) {
  $(`.${parameters.css_featureinfo}`).remove();
  const info_el = document.createElement("div");
  info_el.className = "feature-info";
  $(parameters.plas_map_div_id).prepend(info_el);
  $(`.${parameters.css_featureinfo}`).append(
    '<button type="button" class="close"><i class="material-icons">highlight_off</i></button>'
  );
  $(`.${parameters.css_featureinfo}`).append(`<div>${text}</div>`);
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
const do_expand_point_group = (objects: Objects) =>
  function (event: { target: any }) {
    const f_element = event.target;
    const gr_index = parseInt(
      $(f_element).attr(parameters.group_index_attr) as any
    );
    const g = objects.groups[gr_index];
    const text = text_for_group(g, gr_index + 1);
    return do_feature_expand(f_element, text);
  };

// Mouseover event handler for expanding a regional feature
const regional_feature_information = function (event: { target: any }) {
  const f_element = event.target;
  const text = f_element.getAttribute(parameters.feature_text_attr);
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
const draw_circle = (mapSVG: any) =>
  mapSVG.svg
    .append("g")
    .selectAll("circle")
    .data([0])
    .enter()
    .append("circle")
    .attr("cx", 0)
    .attr("cy", 0)
    .attr("r", parameters.map_radius)
    .style("fill", "none")
    .style("stroke", "#000");

// Draw all of the point features to be displayed
const draw_point_features = function (objects: Objects, mapSVG: any) {
  //TODO: use data
  const grp = mapSVG.svg.append("g");
  pointFeatures(objects).forEach((f) => {
    console.log(f);
    grp
      .append("path")
      .attr("class", `line ${parameters.css_point_feature}`)
      .attr(
        "d",
        (d3.svg as any).line.radial()([
          [parameters.inner_radius, feature_angle(objects, f)],
          [parameters.outer_radius, feature_angle(objects, f)],
        ])
      );
  });
};

// Draw all of the regional features to be displayed
const draw_regional_features = function (objects: Objects, mapSVG: any) {
  const grp = mapSVG.svg.append("g");
  regionalFeatures(objects).forEach((f: any) => {
    console.log(f);
    grp
      .append("path")
      .attr(
        "class",
        `arc ${parameters.css_regional_feature} feature-${f.feature_class}`
      )
      .attr(
        parameters.feature_text_attr,
        `${f.text} (${f.start} - ${f.start + f.length - 1})`
      )
      .attr(
        "d",
        (d3.svg as any)
          .arc()
          .outerRadius(parameters.outer_radius)
          .innerRadius(parameters.inner_radius)
          .startAngle(angle_from_bp(objects, f.start))
          .endAngle(angle_from_bp(objects, f.start + f.length))
      );
  });
  $(`.${parameters.css_regional_feature}`).mouseover(
    regional_feature_information
  );
};

// Draw all of the groups of overlapping point features
const draw_point_groups = function (objects: Objects, mapSVG: any) {
  //TODO: use data
  const grp = mapSVG.svg.append("g");
  const iterable = objects.groups;
  for (let i = 0; i < iterable.length; i++) {
    const g = iterable[i];
    grp
      .append("path")
      .attr("class", `arc ${parameters.css_point_group}`)
      .attr("group-index", i)
      .attr(
        "d",
        (d3.svg as any)
          .arc()
          .outerRadius(
            parameters.outer_radius +
              parameters.point_group_offset +
              parameters.point_group_thickness
          )
          .innerRadius(parameters.outer_radius + parameters.point_group_offset)
          .startAngle(
            angle_from_bp(objects, g[0].at) - parameters.angle_min_thickness
          )
          .endAngle(
            angle_from_bp(objects, g[g.length - 1].at) +
              parameters.angle_min_thickness
          )
      );
  }
  $(`.${parameters.css_point_group}`).mouseover(do_expand_point_group(objects));
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
  const x = x_from_polar(parameters.outer_radius + radial_offset, angle);
  const y =
    y_from_polar(parameters.outer_radius + radial_offset, angle) +
    Math.sin(symm_angle / 2) * 8;
  return grp
    .append("text")
    .attr("class", parameters.css_feature_label)
    .attr("x", x)
    .attr("y", y)
    .attr("text-anchor", calculate_text_anchor_point(angle))
    .text(text)
    .attr("transform", `rotate(${rotation_angle}, ${x}, ${y})`);
};

// Draw all the point feature labels
const draw_point_labels = function (objects: Objects, mapSVG: any) {
  const grp = mapSVG.svg.append("g");
  pointFeatures(objects).forEach((f: any) => {
    if (f.grouped) {
      return;
    }
    const angle = angle_from_bp(objects, f.at);
    const text = `${f.text} (${f.at})`;
    draw_single_feature_label(grp, angle, text);
  });
};

// Draw all the point feature group labels
const draw_group_labels = function (objects: Objects, mapSVG: any) {
  const grp = mapSVG.svg.append("g");
  const iterable = objects.groups;
  for (let i = 0; i < iterable.length; i++) {
    const g = iterable[i];
    const angle =
      (angle_from_bp(objects, g[0].at) +
        angle_from_bp(objects, g[g.length - 1].at)) /
      2;
    const text = `${i + 1}`;
    draw_single_feature_label(grp, angle, text, 10, false);
  }
};

// Draw the plasmid label
const draw_plasmid_label = function (objects: Objects, mapSVG: any) {
  const grp = mapSVG.svg.append("g");
  return grp
    .append("text")
    .attr("class", `${parameters.css_plasmid_label}`)
    .text(`${objects.pl_name} (${objects.pl_size}bp)`)
    .attr("text-anchor", "middle");
};

// Initialize all the elements necessary to draw the map
const initialize_map = function () {
  const mapSVG = {
    svg: d3
      .select(parameters.plas_map_div_id)
      .append("div")
      .attr("class", "chart")
      .style("width", `${parameters.map_width}px`)
      .style("height", `${parameters.display_height}px`)
      .append("svg")
      .attr("width", parameters.map_width)
      .attr("height", parameters.display_height)
      .append("g")
      .attr("id", "offset-group")
      .attr(
        "transform",
        `translate(${parameters.map_width / 2 + parameters.map_offset_x}, ${
          parameters.map_height / 2 +
          parameters.map_offset_y +
          parameters.info_offset_y
        })`
      ),
  };
  update_offset();
  return mapSVG;
};

// Clear the map
const clear_map = () => $(".chart").remove();

// Draw all of the elements of the map
const draw_all = function (objects: Objects) {
  fix_feature_overlap(objects);
  calculate_map_offset();
  update_offset();
  const mapSVG = initialize_map();
  update_offset();
  draw_circle(mapSVG);
  draw_regional_features(objects, mapSVG);
  draw_point_features(objects, mapSVG);
  draw_point_groups(objects, mapSVG);
  draw_point_labels(objects, mapSVG);
  draw_group_labels(objects, mapSVG);
  draw_plasmid_label(objects, mapSVG);
};

// Redraw the map if it has been displayed already
const redraw_map = function (objects: Objects) {
  clear_map();
  return draw_all(objects);
};

//# Feature handling functions

// Helper to ensure that there are at least as many point feature groups as
// necessary
const ensure_n_feature_groups = (objects: Objects, n: number) => {
  while (objects.groups.length < n) {
    objects.groups.push([]);
  }
};

// Adds a feature to the specified group.
// Args:
//   f: the feature to add
//   group_n: the 0-indexed group number; pass a negative number to indicate
//     not grouped
const group_feature = function (objects: Objects, f: Feature, group_n: number) {
  if (group_n < 0) {
    f.grouped = false;
    return;
  }
  ensure_n_feature_groups(objects, group_n + 1);
  objects.groups[group_n].push(f);
  f.grouped = true;
  return (f.group = group_n);
};

// Fixes overlapping point features by adding closely spaced features to a group
const fix_feature_overlap = function (objects: Objects) {
  let f: any;
  objects.groups = [];
  const allDisplayedPointFeatures: PointFeature[] = Object.entries(
    displayedFeatures(objects)
  )
    .filter((entry) => entry[1][0].type === "point")
    .flatMap((entry) => entry[1] as PointFeature[]);
  allDisplayedPointFeatures.sort((a, b) => a.at - b.at);

  let last_start = undefined;
  let last_f = undefined;

  for (f of allDisplayedPointFeatures) {
    //TODO: check for full circle first
    if (last_start !== undefined) {
      if (
        angle_from_bp(objects, f.at) - last_start <
        parameters.min_angular_dist
      ) {
        group_feature(objects, f, objects.groups.length - 1);
      } else {
        last_start = undefined;
        group_feature(objects, f, -1);
      }
    } else {
      if (last_f !== undefined) {
        if (
          angle_from_bp(objects, f.at) - angle_from_bp(objects, last_f.at) <
          parameters.min_angular_dist
        ) {
          last_start = angle_from_bp(objects, last_f.at);
          const new_group_index = objects.groups.length;
          group_feature(objects, last_f, new_group_index);
          group_feature(objects, f, new_group_index);
        } else {
          group_feature(objects, f, -1);
        }
      }
    }
    last_f = f;
  }
};

// Shows a previously hidden feature
const show_feature = function (objects: Objects, name: string) {
  if (!objects.featureNamesToDisplay.includes(name)) {
    objects.featureNamesToDisplay.push(name);
  }
};

// Hides a previously shown feature
const hide_feature = function (objects: Objects, name: string) {
  if (objects.featureNamesToDisplay.includes(name)) {
    objects.featureNamesToDisplay = objects.featureNamesToDisplay.filter(
      (visibleName) => visibleName !== name
    );
  }
};

// Set the visibility of a feature grabbed from the feature field according to # whether the show or hide button was pressed.
const set_feature_from_field = function (objects: Objects, visible: boolean) {
  const name: any = $(parameters.dyn_enz_field).val();
  if (visible) {
    show_feature(objects, name);
  } else {
    hide_feature(objects, name);
  }
  return redraw_map(objects);
};

// Show a feature named by the user in the feature field
const add_feature_from_field = (objects: Objects) =>
  set_feature_from_field(objects, true);

// Hide a feature named by the user in the feature field
const hide_feature_from_field = (objects: Objects) =>
  set_feature_from_field(objects, false);

//# Initialization functions

// Read, parse, and return the json plasmid map data from the plasmid map page
// element
const read_data = function () {
  const data = JSON.parse(
    $(parameters.plas_map_div_id).attr(parameters.data_attr) as any
  );
  return data;
};

// Show the features that should be initially displayed
const with_shown_initial_features = function (o: Objects): Objects {
  const always_on = parameters.initially_displayed_enzymes;

  const findShownFeatures = (features: {
    [name: string]: Feature[];
  }): string[] => {
    const featureNamesToDisplay = [...always_on];
    for (let featureName in features) {
      const currFeatures = features[featureName];
      if (currFeatures.length === 0) {
        continue;
      }
      if (
        currFeatures[0].type === "point" &&
        !enzymes_to_show.has(currFeatures[0].text)
      ) {
        continue;
      }
      if (currFeatures.length === 1 && currFeatures[0].type === "point") {
        featureNamesToDisplay.push(featureName);
      } else if (currFeatures[0].type === "regional") {
        featureNamesToDisplay.push(featureName);
      }
    }
    return featureNamesToDisplay;
  };

  return {
    ...o,
    featureNamesToDisplay: findShownFeatures(o.features),
  };
};

type Objects = {
  data: any;
  featureNamesToDisplay: string[];
  groups: any[];
  pl_size: number;
  pl_name: string;
  features: { [name: string]: Feature[] };
};

function displayedFeatures(o: Objects): { [name: string]: Feature[] } {
  return Object.fromEntries(
    Object.entries(o.features).filter((entry) =>
      o.featureNamesToDisplay.includes(entry[0])
    )
  ) as { [name: string]: Feature[] };
}

// Do all of the feature setup, including reading from the page
const initialize_features = (): Objects => {
  const fs = read_data();
  const o: Objects = {
    data: fs,
    featureNamesToDisplay: [],
    pl_size: fs.pl_size,
    pl_name: fs.pl_name,
    groups: [],
    features: { ...fs["features"] },
  };
  return with_shown_initial_features(o);
};

// Set up the show/hide button click handlers
const initialize_buttons = function (objects: Objects) {
  $(parameters.enz_remove_button).click(() => hide_feature_from_field(objects));
  return $(parameters.enz_add_button).click(() =>
    add_feature_from_field(objects)
  );
};

// Do everything required to make the map
const do_map = function () {
  clear_map();
  const objects = initialize_features();
  initialize_buttons(objects);
  draw_all(objects);
};

export default do_map;
