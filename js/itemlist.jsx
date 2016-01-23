const React = require("react");
const _ = require("underscore");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const ActionExecutors = require("./action-executors.js");
const ss = require("./shared-styles.js");

const extractLookup = function(group, fieldName) {
    const item = _.first(_.where(group, {name: fieldName}));
    return item && item.lookup;
};

function extVal(data, lookup) {
    return lookup && data[lookup];
}

const ItemRow = React.createClass({
    propTypes: {
        clickHandler: React.PropTypes.func,
        data: React.PropTypes.any,  // TODO(colin): what is this?
    },
    loadAssociatedItem: function() {
        const apiBase = this.props.data.dynamicResourceBase;
        const resource = this.props.data.resource;
        const dataURI = `${apiBase}${resource}`;
        window._store.updateDataFromURL(dataURI);
    },

    suppField: function(name) {
        return extVal(
            this.props.data.fieldData,
            extractLookup(
                this.props.data.supplementalFields,
                name));
    },
    render: function() {
        const linkHTML = this.props.data.coreLinks ? <span>
            {this.props.data.coreLinks.links.map((lnk, idx) => {
                const [text, addr] = lnk;
                return <a
                    className={css(styles.coreLink)}
                    href={addr}
                    key={`link${idx}`}
                >
                    {text}
                </a>;
            })}
        </span> : null;
        return <tr
            className={css(styles.itemRow)}
            onClick={this.props.clickHandler}
        >
            <td className={css(styles.itemField)}>
                {this.props.data.name}
            </td>
            <td className={css(styles.itemField)}>
                {this.suppField("Date")}
            </td>
            <td className={css(styles.itemField)}>
                {this.suppField("Entered by")}
            </td>
            <td className={css(styles.itemField)}
                dangerouslySetInnerHTML={{__html: extVal(
                    this.props.data.fieldData,
                    this.props.data.shortDesc.lookup)}}
            >
            </td>
            {this.props.data.coreLinks ?
            <td className={css(styles.itemField)}>
                {linkHTML}
            </td> : null}
        </tr>;
    },
});


const ItemTable = React.createClass({
    propTypes: {
        data: React.PropTypes.any,  // TODO(colin): what is this?
    },
    getInitialState: function() {
        return {
            sort: "id",
            sortOrder: "desc",
        };
    },

    viewItem: function(item) {
        ActionExecutors.maybeFetchThenDisplay(
            "item", _.pick(item, ["type", "resourcePath", "id"]));
    },

    render: function() {
        const sorted = _.sortBy(
            this.props.data.items,
            (x) => x[this.state.sort]);
        if (this.state.sortOrder === "desc") {
            sorted.reverse();
        }
        const items = _.map(sorted, (item) => {
            return <ItemRow
                clickHandler={() => this.viewItem(item)}
                data={item}
                key={item.id}
            />;
        });
        const startItem = extVal(sorted[0].fieldData,
                                 this.props.data.numberFieldName);
        const endItem = extVal(sorted[sorted.length - 1].fieldData,
                               this.props.data.numberFieldName);
        return <div>
            Showing items {startItem}-{endItem}.
            <div className={css(styles.itemtableContainer)}>
                <table className={css(styles.itemtable)}>
                    <thead><tr>
                        <th>ID</th>
                        <th>Date</th>
                        <th>Entered by</th>
                        <th>Name</th>
                        {sorted.length > 0 && sorted[0].coreLinks ?
                        <th>Links</th> : null}
                    </tr></thead>
                    <tbody>
                        {items}
                    </tbody>
                </table>
            </div>
        </div>;
    },
});

const styles = StyleSheet.create({
    coreLink: {
        marginRight: "1em",
        ...ss.elements.link,
    },
    itemField: {
        padding: `${ss.sizes.paddingPx / 2}px ${ss.sizes.paddingPx}px`,
    },
    itemRow: {
        ':nth-of-type(2n)': {
            backgroundColor: ss.colors.lightBackground,
        },
        ':hover': {
            backgroundColor: ss.colors.mediumBackground,
            cursor: "pointer",
        },
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
    },

});

module.exports = ItemTable;
