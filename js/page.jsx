const React = require("react");
const _ = require("underscore");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const Actions = require("./actions.js");
const ActionExecutors = require("./action-executors.js");
const ItemTable = require("./itemlist.jsx");
const ItemInfoView = require("./itemview.jsx");
const Navbar = require("./nav.jsx");
const ss = require("./shared-styles.js");

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
            <div className={css(styles.pageContainerOuter)}>
                <div className={css(styles.pageContainer)}>
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
            </div>
        </div>;
    },
});

const styles = StyleSheet.create({
    pageContainer: {
        margin: "0 auto",
        maxWidth: "80vw",
        padding: 3 * ss.sizes.paddingPx,
        paddingTop: ss.sizes.navbarHeightPx,
    },
    pageContainerOuter: {
        boxSizing: "border-box",
        fontFamily: ss.fonts.base,
        fontSize: ss.sizes.fontSizeNormal,
        height: "100vh",
        overflowX: "hidden",
        overflowY: "auto",
        maxWidth: "100vw",
    },
});

module.exports = Page;
