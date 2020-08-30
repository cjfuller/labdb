import React from "react";
import { StyleSheet, css } from "aphrodite";

import InfoSection from "./info-section";
import InventoryWidget from "./inventory-widget";
import SequenceSection from "./sequence-section";
import ss from "./shared-styles";

type Props = {
  data: any;
  editable: boolean;
  makeUpdater: Function;
  unsavedChanges: any;
};

export default function CoreInfo(props: Props) {
  return (
    <div className={css(styles.coreInfo)}>
      {props.data.coreInfoSections.map((s: any) => {
        return (
          <InfoSection
            data={props.data.fieldData}
            makeUpdater={props.makeUpdater}
            editable={props.editable}
            unsavedChanges={props.unsavedChanges}
            key={s.name}
            name={s.name}
            contents={s}
          />
        );
      })}
      <SequenceSection
        data={props.data.fieldData}
        editable={props.editable}
        sequence={props.data.sequenceInfo}
        unsavedChanges={props.unsavedChanges}
        makeUpdater={props.makeUpdater}
      />
      <InventoryWidget
        data={props.data.fieldData}
        editable={props.editable}
        inventory={props.data.inventory}
        makeUpdater={props.makeUpdater}
        unsavedChanges={props.unsavedChanges}
      />
    </div>
  );
}

const styles = StyleSheet.create({
  coreInfo: {
    border: `1px solid ${ss.colors.borderColor}`,
    borderRadius: ss.sizes.cornerRadiusPx,
    margin: ss.sizes.paddingPx,
    maxWidth: "100%",
    padding: 1.5 * ss.sizes.paddingPx,
  },
});
