const React = require("react");

const ActionExecutors = require("./action-executors.js");
const CoreInfo = require("./core-info.jsx");
const MegaBar = require("./megabar.jsx");
const SupplementalInfo = require("./supplemental-info.jsx");

const ItemInfoView = React.createClass({
    propTypes: {
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        unsavedChanges: React.PropTypes.any, // TODO
    },
    getInitialState: function() {
        return {itemUpdates: {}};
    },

    componentWillReceiveProps: function() {
        this.setState({itemUpdates: {}});
    },

    onUpdate: function(key, value) {
        ActionExecutors.editField(this.props.data.type,
                                  this.props.data.id,
                                  key,
                                  value);
    },

    makeUpdater: function(key) {
        return (value) => this.onUpdate(key, value);
    },

    render: function() {
        return <div className="item-info-view">
            <MegaBar
                data={this.props.data}
                editable={this.props.editable}
                unsavedChanges={this.props.unsavedChanges}
                makeUpdater={this.makeUpdater}
            />
            <div className="row info-row">
                <CoreInfo
                    data={this.props.data}
                    editable={this.props.editable}
                    unsavedChanges={this.props.unsavedChanges}
                    makeUpdater={this.makeUpdater}
                />
                <SupplementalInfo
                    data={this.props.data}
                    editable={this.props.editable}
                    unsavedChanges={this.props.unsavedChanges}
                    makeUpdater={this.makeUpdater}
                />
        </div>
        </div>;
    },
});

module.exports = ItemInfoView;
