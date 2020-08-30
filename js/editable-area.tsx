import React from "react";
import { StyleSheet, css } from "aphrodite";

import ss from "./shared-styles";

type Props = {
  editable: boolean;
  fieldClasses: any;
  onChange: Function;
  value: string;
};

export default function EditableArea(props: Props) {
  return props.editable ? (
    <textarea
      className={css(styles.editableField)}
      disabled={!props.editable}
      onChange={(e) => props.onChange(e.target.value)}
      value={props.value}
    />
  ) : (
    <div>{props.value}</div>
  );
}

const styles = StyleSheet.create({
  editableField: {
    border: "none",
    fontFamily: ss.fonts.monospace,
    minHeight: 200,
    overflowWrap: "break-word",
    width: "100%",
  },
});
