var extractValue = function(group, fieldName) {
    var item = _.first(_.where(group, {name: fieldName}));
    return item && item.value;
};

var ItemRow = React.createClass({

    loadAssociatedItem: function() {
        var apiBase = this.props.data.dynamicResourceBase;
        var resource = this.props.data.resource;
        var dataURI = `${apiBase}${resource}`;
        window._store.updateDataFromURL(dataURI);
    },

    render: function() {
        return <tr className="itemlist" onClick={this.loadAssociatedItem}>
            <td>{this.props.data.name}</td>
            <td>{extractValue(this.props.data.supplementalFields,
                              "Date")}</td>
            <td>{extractValue(this.props.data.supplementalFields,
                              "Entered by")}</td>
            <td>{this.props.data.shortDesc}</td>
            <td>{this.props.data.coreLinks}</td>
            <td>TODO: controls</td>
        </tr>;
    }
});


var ItemTable = React.createClass({
    getInitialState: function() {
        return {
            sort: "id",
            sortOrder: "desc",
        };
    },

    render: function() {
        var sorted = _.sortBy(
            this.props.data.items,
            (x) => x[this.state.sort]);
        if (this.state.sortOrder === "desc") {
            sorted.reverse();
        }
        var items = _.map(sorted, (item) => {
            return <ItemRow key={item.id} data={item} />;
        });
        var startItem = this.props.data.start + 1;
        var endItem = this.props.data.end;
        var count = this.props.data.count;
        return <div className="itemtable-container">
            Showing items {startItem}-{endItem} of {count}.
            <table className="itemtable">
                <thead><tr>
                    <th>ID</th>
                    <th>Date</th>
                    <th>Entered by</th>
                    <th>Name</th>
                    <th>Links</th>
                    <th></th>
                </tr></thead>
                <tbody>
                    {items}
                </tbody>
            </table>
        </div>;
    },
});

window.ItemTable = ItemTable;