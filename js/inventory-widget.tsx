import React from "react";
import { StyleSheet, css } from "aphrodite";

import EditableField from "./editable-field";
import { extVal } from "./util";
import ss from "./shared-styles";

type Props = {
  data: any;
  editable: boolean;
  inventory: any;
  makeUpdater: Function;
  unsavedChanges: any;
};

function getFieldData(props: Props) {
  if (props.editable) {
    return { ...props.data, ...props.unsavedChanges };
  }
  return props.data;
}

function parseInventory(props: Props) {
  if (!props.inventory) {
    return;
  }
  const items: { [name: string]: any }[] = [];
  props.inventory.forEach((field: any) => {
    const valueStr = extVal(getFieldData(props), field.lookup);
    if (!valueStr) {
      return;
    }
    const values = valueStr.split(",");
    values.forEach((val: string, index: number) => {
      if (items.length <= index) {
        items.push({});
      }
      items[index][field.lookup] = val;
    });
  });
  return items;
}

function changeInventory(
  props: Props,
  index: number,
  field: any,
  newValue: any
) {
  const updater = props.makeUpdater(field);
  const origValues = extVal(getFieldData(props), field) || "";
  const arrValues = origValues.split(",");
  arrValues[index] = newValue;
  const nextValue = arrValues.join(",");
  updater(nextValue);
}

function addRow(props: Props) {
  const fieldToUse = "locations";
  const origValue = extVal(getFieldData(props), fieldToUse) || "";
  const nextValue = origValue + ",";
  props.makeUpdater(fieldToUse)(nextValue);
}

function InventoryRows(props: Props & { invItems: any[] | undefined }) {
  const { invItems } = props;

  return (
    <>
      {(invItems ?? []).map((invitem, index) => {
        // TODO: don't tie these fields to a particular implementation.
        return (
          <tr key={index}>
            <td className={css(styles.tcol)}>
              <EditableField
                editable={props.editable}
                onChange={(value: any) =>
                  changeInventory(props, index, "locations", value)
                }
                value={invitem.locations}
              />
            </td>
            <td className={css(styles.tcol)}>
              <EditableField
                editable={props.editable}
                onChange={(value: any) =>
                  changeInventory(props, index, "current_stock_counts", value)
                }
                value={invitem.current_stock_counts}
              />
            </td>
            {/* TODO: figure out how clones are being stored and re-enable.
                <td className={css(styles.tcol)}>
                    <EditableField
                        editable={this.props.editable}
                        onChange={(value) => this.changeInventory(index, 'stock_clone', value)}
                        value={invitem.stock_clone}
                    />
                </td>
                */}
            <td className={css(styles.tcol)}>
              <EditableField
                editable={props.editable}
                onChange={(value: any) =>
                  changeInventory(props, index, "stock_person", value)
                }
                value={invitem.stock_person}
              />
            </td>
            <td className={css(styles.tcol)}>
              <EditableField
                editable={props.editable}
                onChange={(value: any) =>
                  changeInventory(props, index, "stock_date", value)
                }
                value={invitem.stock_date}
              />
            </td>
          </tr>
        );
      })}
    </>
  );
}

export default function InventoryWidget(props: Props) {
  if (!props.inventory) {
    return <div></div>;
  }
  const invItems = parseInventory(props);
  return (
    <div className={css(styles.inventorySection)}>
      <div className={css(styles.inventoryLabel)}>Inventory</div>
      <div className={css(styles.inventoryContent)}>
        <table className={css(styles.inventoryTable)}>
          <thead>
            <tr>
              <th className={css(styles.tcol)}>Location</th>
              <th className={css(styles.tcol)}>Stock count</th>
              {/*<th className={css(styles.tcol)}>Clone</th>*/}
              <th className={css(styles.tcol)}>Person</th>
              <th className={css(styles.tcol)}>Date</th>
            </tr>
          </thead>
          <tbody>
            <InventoryRows invItems={invItems} {...props} />
          </tbody>
        </table>
        {props.editable ? (
          <a
            className={css(styles.addRowButton)}
            href="javascript:void(0)"
            onClick={() => addRow(props)}
          >
            <i className="material-icons">playlist_add</i>Add row
          </a>
        ) : null}
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  inventoryTable: {
    borderCollapse: "collapse",
    textAlign: "left",
  },
  inventoryLabel: {
    ...ss.elements.sectionLabel,
  },
  inventorySection: {
    marginTop: 2 * ss.sizes.paddingPx,
  },
  inventoryContent: {
    ...ss.elements.sectionContents,
  },
  tcol: {
    boxSizing: "border-box",
    ":not(:last-of-type)": {
      borderRight: `1px solid ${ss.colors.borderColor}`,
    },
    paddingLeft: 10,
  },
  decrementButton: {
    ...ss.traits.shadowedButton,
    paddingLeft: 10,
    paddingRight: 10,
    paddingTop: 2,
    paddingBottom: 2,
    border: `1px solid ${ss.colors.borderColor}`,
    borderRadius: 4,
    ":hover": {
      backgroundColor: "#eee",
      cursor: "pointer",
    },
  },
  addRowButton: {
    alignItems: "center",
    backgroundColor: ss.colors.labdbGreen,
    borderRadius: 4,
    boxShadow: "1px 1px 2px rgba(0, 0, 0, 0.2)",
    color: "inherit",
    display: "flex",
    height: 25,
    justifyContent: "center",
    margin: 10,
    textDecoration: "none",
    width: 100,
    ":hover": {
      cursor: "pointer",
      backgroundColor: ss.colors.labdbGreenLight,
    },
    ":active": {
      backgroundColor: ss.colors.labdbGreenLight,
      boxShadow: "inset 1px 1px rgba(0, 0, 0, 0.2)",
    },
  },
});
