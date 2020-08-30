import React, { KeyboardEvent } from "react";
import { StyleSheet, css } from "aphrodite";

import ss from "./shared-styles";

type Props = {
  autoFocus?: boolean | undefined;
  editable: boolean;
  extraStyles?: any[] | undefined;
  fieldClasses?: any | undefined;
  onChange: Function;
  onEnter?: Function | undefined;
  value: any;
};
export default function EditableField(props: Props) {
  const onKeyPress = (event: KeyboardEvent) => {
    if (!props.onEnter) {
      return true;
    }
    if (event.charCode === 13) {
      props.onEnter();
      return false;
    }
    return true;
  };
  return (
    <div className={css(props.editable && styles.editableField)}>
      <input
        autoFocus={!!props.autoFocus}
        className={css(styles.field, ...(props.extraStyles || []))}
        readOnly={!props.editable}
        onChange={(e) => props.onChange(e.target.value)}
        onKeyPress={onKeyPress}
        type="text"
        value={props.value}
      />
    </div>
  );
}

const styles = StyleSheet.create({
  editableField: {
    ...ss.traits.editableBorders,
    ...ss.traits.editableFocus,
  },
  field: {
    ...ss.elements.inputField,
  },
});
