import React from "react";
import { StyleSheet, css } from "aphrodite";

import * as ActionExecutors from "./action-executors";
import EditableText from "./editable-text";
import ss from "./shared-styles";

import { extVal } from "./util";

type FieldProps = {
  data: any;
  editable: boolean;
  item: any;
  onChange: Function;
};

function SupplementalField(props: FieldProps) {
  const f = props.item;
  const labelRef = "label-" + f.name;
  return (
    <div className={css(styles.supplementalItem)}>
      <div className={css(styles.fieldName)} key={labelRef} id={labelRef}>
        {f.name + ":"}
      </div>
      <div aria-labelledby={labelRef} className={css(styles.fieldValue)}>
        <EditableText
          value={extVal(props.data, f.lookup)}
          editable={props.editable}
          onChange={props.onChange}
        />
      </div>
    </div>
  );
}

type Props = {
  data: any;
  editable: boolean;
  makeUpdater: Function;
  unsavedChanges: any;
};

export default function SupplementalInfo(props: Props) {
  const getFieldData = () => {
    if (props.editable) {
      return { ...props.data.fieldData, ...props.unsavedChanges };
    }
    return props.data.fieldData;
  };

  return (
    <div className={css(styles.supplementWrapper)}>
      <div className={css(styles.supplementalInfo)}>
        {props.data.supplementalFields.map((f: any) => {
          return (
            <SupplementalField
              data={getFieldData()}
              editable={props.editable}
              item={f}
              key={f.name}
              onChange={props.makeUpdater(f.lookup)}
            />
          );
        })}
      </div>
      {props.data.type === "plasmid" ? (
        // TODO: thread click callback through props.
        <div
          className={css(styles.plasmapButton)}
          onClick={() => ActionExecutors.showPlasmidMap(props.data)}
          role="button"
        >
          Plasmid map
        </div>
      ) : null}
    </div>
  );
}

const styles = StyleSheet.create({
  fieldName: {
    ...ss.elements.fieldName,
  },
  fieldValue: {
    fontFamily: ss.fonts.base,
    fontSize: ss.sizes.fontSizeNormal,
  },
  plasmapButton: {
    alignItems: "center",
    border: `1px solid ${ss.colors.borderColor}`,
    borderRadius: ss.sizes.cornerRadiusPx,
    boxShadow: `1px 1px 1px ${ss.colors.borderColor}`,
    boxSizing: "border-box",
    display: "flex",
    height: ss.sizes.buttonHeightPx,
    justifyContent: "center",
    margin: ss.sizes.paddingPx,
    textAlign: "center",
    width: "100%",
    ":active": {
      backgroundColor: ss.colors.mediumBackground,
      boxShadow: `inset 1px 1px ${ss.colors.borderColor}`,
    },
    ":hover": {
      backgroundColor: ss.colors.lightBackground,
      cursor: "pointer",
    },
  },
  supplementalInfo: {
    border: `1px solid ${ss.colors.borderColor}`,
    borderRadius: ss.sizes.cornerRadiusPx,
    boxSizing: "border-box",
    margin: ss.sizes.paddingPx,
    padding: 1.5 * ss.sizes.paddingPx,
    width: "100%",
  },
  supplementalItem: {
    ":not(:last-of-type)": {
      marginBottom: ss.sizes.paddingPx,
    },
  },
  supplementWrapper: {
    boxSizing: "border-box",
    display: "block",
    width: "100%",
  },
});
