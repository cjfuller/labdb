import React from "react";
import { StyleSheet, css } from "aphrodite";

import EditableBoolean from "./editable-boolean";
import EditableText from "./editable-text";
import { extVal } from "./util";
import ss from "./shared-styles";

type Props = {
  contents: any;
  data: any;
  editable: boolean;
  makeUpdater: Function;
  name: string;
  unsavedChanges: any;
};

function SectionContents(props: Props) {
  const val = (l: string) =>
    extVal({ ...props.data, ...props.unsavedChanges }, l);
  if (props.contents.preformatted) {
    return props.editable ? (
      <div key="editable">
        <EditableText
          value={val(props.contents.lookup)}
          editable={props.editable}
          single={props.contents.single}
          onChange={props.makeUpdater(props.contents.lookup)}
        />
      </div>
    ) : (
      <div
        className={css(styles.fieldValue)}
        dangerouslySetInnerHTML={{
          __html: props.contents.inlineValue,
        }}
        key="prerendered"
      />
    );
  }
  return props.contents.fields.map(
    (f: { name: string; type: string; value: any; lookup: string }) => {
      const isBooleanField = f.type && f.type === "boolean";
      const sep = isBooleanField ? "?" : ":";

      const labelRef = "label-" + f.name;
      return (
        <div className={css(styles.fieldContainer)} key={f.name}>
          <div className={css(styles.fieldName)} key={labelRef} id={labelRef}>
            {f.name + sep}
          </div>
          <div
            aria-labelledby={labelRef}
            className={css(
              props.editable && styles.editableField,
              styles.fieldValue,
              props.contents.single && styles.major
            )}
            key={"value-" + f.name}
          >
            {isBooleanField ? (
              <EditableBoolean
                editable={props.editable}
                onChange={props.makeUpdater(f.lookup)}
                unknownMarker="?"
                value={val(f.lookup)}
              />
            ) : (
              <EditableText
                editable={props.editable}
                onChange={props.makeUpdater(f.lookup)}
                value={val(f.lookup)}
              />
            )}
          </div>
        </div>
      );
    }
  );
}

export default function InfoSection(props: Props) {
  const label = `sectionLabel-${props.name}`;
  return (
    <div className={css(styles.infoSection)}>
      <div className={css(styles.infoSectionLabel)} id={label}>
        {props.name}
      </div>
      <div aria-labelledby={label} className={css(styles.infoSectionContents)}>
        <SectionContents {...props} />
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  fieldContainer: {
    display: "flex",
    justifyContent: "flex-start",
  },
  fieldName: {
    flexShrink: 0,
    ...ss.elements.fieldName,
  },
  infoSection: {
    ":not(:last-of-type)": {
      marginBottom: 2 * ss.sizes.paddingPx,
    },
  },
  infoSectionContents: {
    ...ss.elements.sectionContents,
  },
  infoSectionLabel: {
    ...ss.elements.sectionLabel,
  },
  editableField: {
    ...ss.traits.editableBorders,
    ...ss.traits.editableFocus,
  },
  major: {
    maxWidth: "100%",
    width: "100%",
  },
  fieldValue: {
    display: "inline-block",
    flexBasis: "100%",
    flexShrink: 1,
    overflowWrap: "break-word",
    maxWidth: "100%",
  },
});
