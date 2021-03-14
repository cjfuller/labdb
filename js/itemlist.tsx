import React from "react";
import type { MouseEvent } from "react";
import _ from "underscore";
import { StyleSheet, css } from "aphrodite";

import * as ActionExecutors from "./action-executors";
import ss from "./shared-styles";

const extractLookup = function (group: any, fieldName: any) {
  const item = _.first(_.where(group, { name: fieldName }));
  return item && item.lookup;
};

function extVal(data: { [x: string]: any }, lookup: string) {
  return lookup && data[lookup];
}

type RowProps = {
  clickHandler: (e: MouseEvent) => void;
  data: any;
};

function ItemRow(props: RowProps) {
  const suppField = (name: string) => {
    return extVal(
      props.data.fieldData,
      extractLookup(props.data.supplementalFields, name)
    );
  };

  const linkHTML = props.data.coreLinks ? (
    <span>
      {props.data.coreLinks.links.map((lnk: [any, any], idx: any) => {
        if (!lnk) {
          return "";
        }
        const [text, addr] = lnk;
        return (
          <a className={css(styles.coreLink)} href={addr} key={`link${idx}`}>
            {text}
          </a>
        );
      })}
    </span>
  ) : null;
  return (
    <tr className={css(styles.itemRow)} onClick={props.clickHandler}>
      <td className={css(styles.itemField)}>{props.data.name}</td>
      <td className={css(styles.itemField)}>{suppField("Date")}</td>
      <td className={css(styles.itemField)}>{suppField("Entered by")}</td>
      <td
        className={css(styles.itemField)}
        dangerouslySetInnerHTML={{
          __html: extVal(props.data.fieldData, props.data.shortDesc.lookup),
        }}
      ></td>
      {props.data.coreLinks ? (
        <td className={css(styles.itemField)}>{linkHTML}</td>
      ) : null}
    </tr>
  );
}

type Props = {
  data: any;
  sort?: undefined | string[];
};

export default function ItemTable(props: Props) {
  const viewItem = (item: any) => {
    ActionExecutors.maybeFetchThenDisplay(
      "item",
      _.pick(item, ["type", "resourcePath", "id"])
    );
  };
  const sorted = props.data.items;
  if (sorted.length === 0) {
    return <div className={css(styles.itemsShown)}>No results found.</div>;
  }
  const items = _.map(sorted, (item) => {
    return (
      <ItemRow
        clickHandler={() => viewItem(item)}
        data={item}
        key={item.type + item.id}
      />
    );
  });
  const startItem = extVal(sorted[0].fieldData, props.data.numberFieldName);
  const endItem = extVal(
    sorted[sorted.length - 1].fieldData,
    props.data.numberFieldName
  );
  return (
    <div>
      {props.data.type === "collection" ? (
        <div className={css(styles.itemsShown)}>
          Showing items {startItem}-{endItem}.
        </div>
      ) : null}
      <div className={css(styles.itemtableContainer)}>
        <table className={css(styles.itemtable)}>
          <thead>
            <tr>
              <th>ID</th>
              <th>Date</th>
              <th>Entered by</th>
              <th>Name</th>
              {sorted.length > 0 && sorted[0].coreLinks ? <th>Links</th> : null}
            </tr>
          </thead>
          <tbody>{items}</tbody>
        </table>
      </div>
    </div>
  );
}
const styles = StyleSheet.create({
  coreLink: {
    marginRight: "1em",
    ...ss.elements.link,
  },
  itemField: {
    ":not(:last-of-type)": {
      paddingRight: ss.sizes.paddingPx * 2,
    },
    paddingTop: ss.sizes.paddingPx / 2,
    paddingBottom: ss.sizes.paddingPx / 2,
  },
  itemRow: {
    ":nth-of-type(2n)": {
      backgroundColor: ss.colors.lightBackground,
    },
    ":hover": {
      backgroundColor: ss.colors.mediumBackground,
      cursor: "pointer",
    },
  },
  itemsShown: {
    marginBottom: 10,
    marginTop: 10,
  },
  itemtableContainer: {
    display: "flex",
    justifyContent: "center",
  },
  itemtable: {
    borderCollapse: "collapse",
    borderSpacing: 0,
    fontFamily: ss.fonts.content,
    fontSize: ss.sizes.fontSizeMedium,
    textAlign: "left",
  },
});
