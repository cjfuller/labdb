const React = require("react");

const DecrementButton = React.createClass({
    propTypes: {
        clickCallback: React.PropTypes.func,
    },
    render: function() {
        return <div
            className="decrement-button"
            onClick={this.props.clickCallback}
        >
            Remove one stock
        </div>;
    },
});

const InventoryWidget = React.createClass({
    propTypes: {
        inventory: React.PropTypes.arrayOf(
            React.PropTypes.shape({
                count: React.PropTypes.number,
                clone: React.PropTypes.string,
                date: React.PropTypes.string,
                location: React.PropTypes.string,
                person: React.PropTypes.string,
            })),
    },
    inventoryDecrementAction: function(invitem) {
        return function() {
            $.ajax({
                // TODO: don't rely on window location here.
                url: (
                    window.location.pathname +
                    "/update_number?inc=-1&location=" +
                    JSON.stringify(invitem)),
                method: "PUT",
                // TODO: this redirects to the page we were already on, but we
                // really should ditch the redirect.
                success: function(data, status) {
                    window.location.reload();
                },
                error: function(xhr, status, errorthrown) {
                    window.location.reload();
                },
                async: false,
            });
        };
    },

    render: function() {
        if (!this.props.inventory) {
            return <div></div>;
        }
        return <table className="inventory-table">
        <thead>
            <tr>
                <th>Location</th>
                <th>Stock count</th>
                <th>Clone</th>
                <th>Person</th>
                <th>Date</th>
                <th></th>
            </tr>
        </thead>
        <tbody>
        {this.props.inventory.map((invitem) => {
            return <tr>
                <td>{invitem.location}</td>
                <td>{invitem.count}</td>
                <td>{invitem.clone}</td>
                <td>{invitem.person}</td>
                <td>{invitem.date}</td>
                <td>
                    <DecrementButton
                        clickCallback={this.inventoryDecrementAction(invitem)}
                    />
                </td>
            </tr>;
        })}
        </tbody>
        </table>;
    },
});

module.exports = InventoryWidget;
