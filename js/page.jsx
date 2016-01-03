const React = require("react");
const _ = require("underscore");

const Actions = require("actions");
const ActionExecutors = require("action-executors");
const ItemTable = require("itemlist");
const ItemInfoView = require("itemview");
const Navbar = require("nav");

const Page = React.createClass({
    propTypes: {
        data: React.PropTypes.any,
        dispatch: React.PropTypes.func,
        editMode: React.PropTypes.bool,
        unsavedChanges: React.PropTypes.any,
    },
    editToggle: function() {
        if (this.props.editMode &&
            Object.keys(this.props.unsavedChanges).length > 0) {
            ActionExecutors.saveEdits(
                _.pick(this.props.data, ["type", "id", "resourcePath"]),
                       this.props.unsavedChanges);
        } else if (!this.props.editMode) {
            this.props.dispatch(Actions.setEditMode(true));
        } else {
            this.props.dispatch(Actions.setEditMode(false));
        }
    },

    cancelEdits: function() {
        ActionExecutors.clearEdits(
            _.pick(this.props.data, ["type", "id", "resourcePath"]));
    },

    toggleBurger: function() {
        console.log("Hamburger!");
    },

    render: function() {
        return <div id="page">
            <Navbar
                cancelEditCallback={this.cancelEdits}
                data={this.props.data}
                editCallback={this.editToggle}
                editMode={this.props.editMode}
                onClickHamburger={this.toggleBurger}
            />
            <div className="container">
                {this.props.data.type === "collection" ?
                    <ItemTable
                        data={this.props.data}
                        dispatch={this.props.dispatch}
                    /> :
                    <ItemInfoView
                        data={this.props.data}
                        editable={this.props.editMode}
                        dispatch={this.props.dispatch}
                        unsavedChanges={this.props.unsavedChanges}
                    />}
            </div>
        </div>;
    },
});

module.exports = Page;
