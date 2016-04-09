const React = require("react");
const {StyleSheet, css} = require("aphrodite");
const $ = require("jquery");

const EditableField = require("./editable-field.jsx");
const {extVal} = require("./util.js");
const ss = require("./shared-styles.js");

const DecrementButton = React.createClass({
    propTypes: {
        clickCallback: React.PropTypes.func,
    },
    render: function() {
        return <div
            className={css(styles.decrementButton)}
            onClick={this.props.clickCallback}
        >
            -1
        </div>;
    },
});

const InventoryItem = React.createClass({
    propTypes: {
        itemData: React.PropTypes.any,
    },

    render: function() {
        return <div>{JSON.stringify(this.props.itemData)}</div>;
    },
});

const InventoryWidget = React.createClass({
    propTypes: {
        data: React.PropTypes.any,  // TODO
        editable: React.PropTypes.bool,
        inventory: React.PropTypes.any,  // TODO
        makeUpdater: React.PropTypes.func,
        unsavedChanges: React.PropTypes.any,  // TODO
    },

    getFieldData: function() {
        if (this.props.editable) {
            return {...this.props.data, ...this.props.unsavedChanges};
        }
        return this.props.data;
    },

    parseInventory: function() {
        if (!this.props.inventory) {
            return;
        }
        const items = [];
        this.props.inventory.forEach((field) => {
            const valueStr = extVal(this.getFieldData(), field.lookup);
            if (!valueStr) {return;}
            const values = valueStr.split(",");
            values.forEach((val, index) => {
                if (items.length <= index) {
                    items.push({});
                }
                items[index][field.lookup] = val;
            });
        });
        return items;
    },

    changeInventory: function(index, field, newValue) {
        const updater = this.props.makeUpdater(field);
        const origValues = extVal(this.getFieldData(), field) || "";
        const arrValues = origValues.split(",");
        arrValues[index] = newValue;
        const nextValue = arrValues.join(",");
        updater(nextValue);
    },

    getInventoryRows: function(invItems) {
        return invItems.map((invitem, index) => {
            // TODO: don't tie these fields to a particular implementation.
            return <tr key={index} className={css(styles.trow)}>
                <td className={css(styles.tcol)}>
                    <EditableField
                        editable={this.props.editable}
                        onChange={(value) => this.changeInventory(index, 'locations', value)}
                        value={invitem.locations}
                    />
                </td>
                <td className={css(styles.tcol)}>
                    <EditableField
                        editable={this.props.editable}
                        onChange={(value) => this.changeInventory(index, 'current_stock_counts', value)}
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
                        editable={this.props.editable}
                        onChange={(value) => this.changeInventory(index, 'stock_person', value)}
                        value={invitem.stock_person}
                    />
                </td>
                <td className={css(styles.tcol)}>
                    <EditableField
                        editable={this.props.editable}
                        onChange={(value) => this.changeInventory(index, 'stock_date', value)}
                        value={invitem.stock_date}
                    />
                </td>
             </tr>;
        });
    },

    render: function() {
        if (!this.props.inventory) {
            return <div></div>;
        }
        const invItems = this.parseInventory();
        return <div className={css(styles.inventorySection)}>
            <div className={css(styles.inventoryLabel)}>
                Inventory
            </div>
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
                        {this.getInventoryRows(invItems)}
                    </tbody>
                </table>
            </div>
        </div>;
    },
});

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
        ':not(:last-of-type)': {
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
        ':hover': {
            backgroundColor: "#eee",
            cursor: "pointer",
        },
    },
});


module.exports = InventoryWidget;
