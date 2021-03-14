import React from "react";
import { StyleSheet, css } from "aphrodite";

import * as ActionExecutors from "./action-executors";
import CoreInfo from "./core-info";
import MegaBar from "./megabar";
import ss from "./shared-styles";
import SupplementalInfo from "./supplemental-info";

type Props = {
  data: any;
  editable: boolean;
  unsavedChanges: any;
};

export default function ItemInfoView(props: Props) {
  const onUpdate = (key: any, value: any) => {
    ActionExecutors.editField(props.data.type, props.data.id, key, value);
  };

  const makeUpdater = (key: any) => (value: any) => onUpdate(key, value);
  return (
    <div className={css(styles.itemInfoView)}>
      <MegaBar
        data={props.data}
        editable={props.editable}
        unsavedChanges={props.unsavedChanges}
        makeUpdater={makeUpdater}
      />
      <div className={css(styles.infoRow)}>
        <div className={css(styles.coreInfoContainer)}>
          <CoreInfo
            data={props.data}
            editable={props.editable}
            unsavedChanges={props.unsavedChanges}
            makeUpdater={makeUpdater}
          />
        </div>
        <div className={css(styles.supplementalInfoContainer)}>
          <SupplementalInfo
            data={props.data}
            editable={props.editable}
            unsavedChanges={props.unsavedChanges}
            makeUpdater={makeUpdater}
          />
        </div>
      </div>
    </div>
  );
}

const smallScreen = "@media(max-width: 899px)";

const styles = StyleSheet.create({
  coreInfoContainer: {
    flexGrow: 3,
    flexShrink: 3,
    maxWidth: "85%",
    [smallScreen]: {
      maxWidth: "100%",
    },
  },
  itemInfoView: {
    fontFamily: ss.fonts.content,
    fontSize: ss.sizes.fontSizeMedium,
  },
  infoRow: {
    alignItems: "flex-start",
    display: "flex",
    flexDirection: "row",
    marginBottom: 1.5 * ss.sizes.paddingPx,
    maxWidth: "100%",
    [smallScreen]: {
      flexDirection: "column",
    },
  },
  supplementalInfoContainer: {
    flexGrow: 1,
    flexShrink: 1,
  },
});
