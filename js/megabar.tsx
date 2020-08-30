import React from "react";
import { StyleSheet, css } from "aphrodite";

import EditableText from "./editable-text";
import { extVal } from "./util";
import ss from "./shared-styles";

type Props = {
  data: {
    coreLinks: any;
    fieldData: any;
    name: string;
    shortDesc: any;
  };
  editable: boolean;
  makeUpdater: Function;
  unsavedChanges: any;
};

export default function MegaBar(props: Props) {
  const getFieldData = () => {
    if (props.editable) {
      return { ...props.data.fieldData, ...props.unsavedChanges };
    }
    return props.data.fieldData;
  };

  const getCoreLinks = () => {
    if (props.data.coreLinks) {
      return (
        <div className={css(styles.fieldValue)}>
          {props.editable ? (
            <EditableText
              editable={props.editable}
              onChange={props.makeUpdater(props.data.coreLinks.lookup)}
              value={extVal(getFieldData(), props.data.coreLinks.lookup)}
            />
          ) : (
            props.data.coreLinks.links.map((link: any, i: number) => {
              const [text, href] = link;
              return (
                <a className={css(styles.coreLink)} href={href} key={text}>
                  {text}
                </a>
              );
            })
          )}
        </div>
      );
    }
    return null;
  };
  const editingString = props.editable ? "Editing " : "";
  return (
    <div className={css(styles.itemMegabar)}>
      <div className={css(styles.itemID)}>
        {editingString + props.data.name}
      </div>
      <div className={css(styles.subtitleContainer)}>
        <div className={css(styles.subtitle)}>
          <div className={css(styles.alias)}>
            {props.editable ? (
              <div className={css(styles.fieldName)}>
                {props.data.shortDesc.name}
              </div>
            ) : null}
            <div className={css(styles.fieldValue)}>
              {props.editable ? (
                <EditableText
                  editable={props.editable}
                  onChange={props.makeUpdater(props.data.shortDesc.lookup)}
                  value={extVal(getFieldData(), props.data.shortDesc.lookup)}
                />
              ) : (
                <div
                  className="item-alias"
                  dangerouslySetInnerHTML={{
                    __html: props.data.shortDesc.inlineValue,
                  }}
                />
              )}
            </div>
          </div>
          <div className={css(styles.linkedItems)}>
            {props.editable ? (
              <div className={css(styles.fieldName)}>
                {(props.data.coreLinks || {}).name}
              </div>
            ) : null}
            {getCoreLinks()}
          </div>
        </div>
        <div className={css(styles.placeholder)}></div>
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  alias: {
    display: "flex",
    flexDirection: "row",
    justifyContent: "flex-start",
    fontFamily: ss.fonts.monospace,
    fontSize: ss.sizes.fontSizeExtraLarge,
    minWidth: 400,
  },
  coreLink: {
    marginRight: ss.sizes.paddingPx,
    ...ss.elements.link,
  },
  fieldName: {
    whiteSpace: "nowrap",
    ...ss.elements.fieldName,
  },
  fieldValue: {
    display: "inline-block",
    width: "100%",
  },
  itemID: {
    fontSize: "3rem",
    fontFamily: ss.fonts.contrast,
  },
  itemMegabar: {
    margin: 2 * ss.sizes.paddingPx,
  },
  link: {
    ...ss.elements.link,
  },
  linkedItems: {
    display: "flex",
    flexDirection: "row",
    justifyContent: "flex-end",
    fontFamily: ss.fonts.base,
    fontSize: ss.sizes.fontSizeLarge,
  },
  placeholder: {
    flex: 1,
  },
  subtitle: {
    display: "flex",
    flex: 3,
    flexDirection: "row",
    justifyContent: "space-between",
  },
  subtitleContainer: {
    display: "flex",
    flexDirection: "row",
    justifyContent: "space-between",
  },
});
