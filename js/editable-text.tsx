import React from "react";

import EditableArea from "./editable-area";
import EditableField from "./editable-field";

export default function EditableText(props: { single: boolean } & any) {
  return props.single ? (
    <EditableArea {...props} />
  ) : (
    <EditableField {...props} />
  );
}
