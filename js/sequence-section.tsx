import React from "react";
import { StyleSheet, css } from "aphrodite";

import EditableBoolean from "./editable-boolean";
import EditableText from "./editable-text";
import { extVal } from "./util";
import ss from "./shared-styles";

type Props = {
  data: any;
  editable: boolean;
  makeUpdater: Function;
  sequence?:
    | undefined
    | {
        sequence: {
          lookup: string;
        };
        verified: {
          lookup: string;
        };
      };
  unsavedChanges: any;
};

export default function SequenceSection(props: Props) {
  const val = (l: string | undefined) =>
    extVal({ ...props.data, ...props.unsavedChanges }, l);

  if (!props.sequence) {
    return <div></div>;
  }
  const verifiedLookup = (props.sequence.verified || {}).lookup;
  return (
    <div className="sequence-section info-section">
      <div className={css(styles.labelRow)}>
        <div className="seq-label-and-size">
          <div className={css(styles.sequenceSectionLabel)}>Sequence</div>
          <div className={css(styles.sequenceSize)}>
            &nbsp;(
            {(val(props.sequence.sequence.lookup) || []).length}bp)
          </div>
        </div>
        <div className="seq-verified">
          {verifiedLookup && props.editable ? (
            <div className="field-name">Verified</div>
          ) : null}
          {verifiedLookup ? (
            <EditableBoolean
              editable={props.editable}
              onChange={props.makeUpdater(verifiedLookup)}
              value={val(verifiedLookup)}
            />
          ) : null}
        </div>
      </div>
      <div
        aria-labelledby="seqLabel"
        className={css(styles.sequenceSectionContents)}
      >
        {/* Was: sequence field-value single major */}
        <div className={css(styles.sequence)}>
          <EditableText
            editable={props.editable}
            onChange={props.makeUpdater(props.sequence.sequence.lookup)}
            single={true}
            value={val(props.sequence.sequence.lookup)}
          />
        </div>
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  labelRow: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
  },
  sequenceSectionContents: {
    ...ss.elements.sectionContents,
  },
  sequenceSectionLabel: {
    display: "inline-block",
    ...ss.elements.sectionLabel,
  },
  sequenceSize: {
    display: "inline-block",
  },
  sequence: {
    fontFamily: ss.fonts.monospace,
    width: "100%",
    wordBreak: "break-all",
    wordWrap: "break-word",
  },
});
