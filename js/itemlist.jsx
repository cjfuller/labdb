const React = require("react");
const _ = require("underscore");

const ActionExecutors = require("action-executors");

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
        const linkHTML = <span>
            {this.props.data.coreLinks.inlineValue.map((inlVal, idx) => {
                return <span
                    dangerouslySetInnerHTML={{__html: inlVal}}
                    key={`link${idx}`}
                    style={{marginRight: "1em"}}
                />;
            })}
        </span>;
        return <tr className="itemlist" onClick={this.props.clickHandler}>
            <td>
                {this.props.data.name}
            </td>
            <td>{this.suppField("Date")}</td>
            <td>{this.suppField("Entered by")}</td>
            <td dangerouslySetInnerHTML={{__html: extVal(
                this.props.data.fieldData,
                this.props.data.shortDesc.lookup)}}
            >
            </td>
            <td>{linkHTML}</td>
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

        return <div className="itemtable-container">
            Showing items {startItem}-{endItem}.
            <table className="itemtable">
                <thead><tr>
                    <th>ID</th>
                    <th>Date</th>
                    <th>Entered by</th>
                    <th>Name</th>
                    <th>Links</th>
                </tr></thead>
                <tbody>
                    {items}
                </tbody>
            </table>
        </div>;
    },
});

module.exports = ItemTable;
