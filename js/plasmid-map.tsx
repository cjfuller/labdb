import React, { useEffect, MouseEvent } from "react";
import { StyleSheet, css } from "aphrodite";

import Modal from "./modal";
import showMap from "./plasmid-map-legacy";
import ss from "./shared-styles";

type Props = {
  data: any;
  onClose: (e: MouseEvent) => void;
  plasmid: any;
};

export default function PlasmidMap(props: Props) {
  useEffect(() => {
    showMap();
  }, []);

  const dataToLegacyFormat = (data: any) => {
    // The new data format is a list of features.  Each feature has:
    // {
    //      feature: <more data on the feature; we need .name>,
    //      pos: the start (or only position),
    //      featureClass: restriction/protein/protease,
    //      featureExtent: point/regional
    // }
    // The old data format was:
    // {
    //      point_features: {
    //          <name>: [
    //              {
    //                  feature_class: restriction / affinity tag /
    //                      protease,
    //                  text: <the feature name>
    //                  Either:
    //                      length: <int>
    //                      start: <int>
    //                      type: "regional"
    //                  or:
    //                      at: <int>
    //                      count: <total number of these in the map>
    //                      type: "point"
    //              }
    //          ]
    //      }
    //      regional_features: {...}
    //
    // This function converts between the two.
    const pointFeatures = data.filter(
      (f: any) =>
        f.featureExtent === "point" && f.featureClass === "restriction"
    );
    const regionalFeatures = data
      .filter((f: any) => f.featureExtent === "regional")
      .concat(
        data.filter(
          (f: any) =>
            f.featureExtent === "point" && f.featureClass === "protease"
        )
      );

    const reformattedPointFeatures = pointFeatures.map((f: any) => {
      return {
        feature_class: f.feature.display["feature_class"] || f.featureClass,
        text: f.feature.name,
        at: f.pos,
        count: null,
        type: "point",
      };
    });

    const reformattedRegionalFeatures = regionalFeatures.map((f: any) => {
      return {
        feature_class: f.feature.display["feature_class"],
        text: f.feature.name,
        start: f.pos,
        length:
          f.featureExtent === "point"
            ? f.feature.sequence.length * 3
            : f.length,
        type: "regional",
      };
    });

    const groupByName = function (features: any) {
      const groups: any = {};
      features.forEach((f: any) => {
        if (groups[f.text] === undefined) {
          groups[f.text] = [];
        }
        groups[f.text] = groups[f.text].concat(f);
      });
      return groups;
    };

    const mapData = {
      pl_name: props.plasmid.name,
      pl_size: (props.plasmid.fieldData.sequence || "").length,
      features: groupByName([
        ...reformattedPointFeatures,
        ...reformattedRegionalFeatures,
      ]),
    };

    // finally, we need to set the count parameter on the point features.
    Object.keys(mapData.features).forEach((group) => {
      const groupValues = mapData.features[group];
      if (groupValues[0].type === "point") {
        groupValues.forEach((f: any) => (f.count = groupValues.length));
      }
    });
    return mapData;
  };
  const data = dataToLegacyFormat(props.data);
  console.log(data);
  // TODO: reactify!
  return (
    <Modal onClose={props.onClose}>
      <div className={css(styles.modalContent)}>
        <div
          className={css(styles.plasmidMap)}
          data-mapinfo={JSON.stringify(data)}
          id="plasmid-map"
        />
        <div className={css(styles.modalFooter)}>
          <input className={css(styles.enzymeInput)} id="enzyme" type="test" />
          <button className={css(styles.showEnzyme)} id="show_enzyme">
            Show enzyme
          </button>
          <button className={css(styles.hideEnzyme)} id="hide_enzyme">
            Hide enzyme
          </button>
        </div>
      </div>
    </Modal>
  );
}

const controlMargin = 5;

const styles = StyleSheet.create({
  enzymeInput: {
    marginLeft: controlMargin,
    marginRight: controlMargin,
  },
  modalContent: {
    width: "100%",
    height: "100%",
    display: "flex",
    alignItems: "center",
    justifyContent: "space-between",
    flexDirection: "column",
  },
  modalFooter: {
    width: "100%",
    display: "flex",
    alignItems: "center",
    justifyContent: "flex-start",
  },
  plasmidMap: {
    display: "flex",
    fontSize: ss.sizes.fontSizeCaption,
    justifyContent: "center",
    alignItems: "center",
    flexDirection: "column",
  },
  hideEnzyme: {
    marginLeft: controlMargin,
    marginRight: controlMargin,
  },
  showEnzyme: {
    marginLeft: controlMargin,
    marginRight: controlMargin,
  },
});
