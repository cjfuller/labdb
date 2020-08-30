import React from "react";
import { StyleSheet, css } from "aphrodite";

import ss from "./shared-styles";

type EditingProps = {
  onChange: Function;
  value: boolean | null | undefined;
};

function BooleanEditingControl(props: EditingProps) {
  const { onChange, value } = props;
  return (
    <div className={css(styles.booleanEditor)}>
      <div
        className={css(
          styles.booleanEditorOption,
          value === true && styles.selectedYes
        )}
        onClick={() => onChange(true)}
      >
        {"\u2714"}
      </div>
      <div
        className={css(
          styles.booleanEditorOption,
          value === false && styles.selectedNo
        )}
        onClick={() => onChange(false)}
      >
        {"\u2716"}
      </div>
      <div
        className={css(
          styles.booleanEditorOption,
          (value === null || value === undefined) && styles.selectedUnknown
        )}
        onClick={() => onChange(null)}
      >
        ?
      </div>
    </div>
  );
}

type Props = {
  editable: boolean;
  unknownMarker: string | null | undefined;
  value: boolean | null | undefined;
  onChange: Function;
};

function IconValue(props: {
  v: boolean | null | undefined;
  unknownMarker: string | null | undefined;
}) {
  const { v, unknownMarker } = props;
  if (v === true) {
    return (
      <div className="yes-icon">
        <i className="material-icons">check</i>
      </div>
    );
  } else if (v === false) {
    return (
      <div className="no-icon">
        <i className="material-icons">close</i>
      </div>
    );
  } else {
    return <div className="unknown-icon">{unknownMarker ?? ""}</div>;
  }
}

export default function EditableBoolean(props: Props) {
  return props.editable ? (
    <BooleanEditingControl {...props} />
  ) : (
    <IconValue unknownMarker={props.unknownMarker} v={props.value} />
  );
}

const styles = StyleSheet.create({
  booleanEditor: {
    display: "inline-block",
    border: `1px solid ${ss.colors.bitDarkBackground}`,
    borderRadius: ss.sizes.cornerRadiusPx,
    marginTop: ss.sizes.paddingPx / 2,
    marginBottom: ss.sizes.paddingPx / 2,
    ...ss.traits.shadowedButton,
  },
  booleanEditorOption: {
    display: "inline-block",
    minWidth: 30,
    textAlign: "center",
    ":hover": {
      backgroundColor: ss.colors.borderColor,
      cursor: "pointer",
    },
  },
  selectedYes: {
    backgroundColor: ss.colors.yesGreen,
    ":hover": {
      backgroundColor: ss.colors.yesGreen,
    },
  },
  selectedNo: {
    backgroundColor: ss.colors.noRed,
    ":hover": {
      backgroundColor: ss.colors.noRed,
    },
  },
  selectedUnknown: {
    backgroundColor: ss.colors.ambiguousBlue,
    ":hover": {
      backgroundColor: ss.colors.ambiguousBlue,
    },
  },
});
