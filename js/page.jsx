const React = require("react");
const _ = require("underscore");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const Actions = require("./actions.js");
const ActionExecutors = require("./action-executors.js");
const Hamburger = require("./hamburger.jsx");
const ItemTable = require("./itemlist.jsx");
const ItemInfoView = require("./itemview.jsx");
const Navbar = require("./nav.jsx");
const ss = require("./shared-styles.js");

const Page = React.createClass({
    propTypes: {
        data: React.PropTypes.any,
        dispatch: React.PropTypes.func,
        editMode: React.PropTypes.bool,
        showHamburger: React.PropTypes.bool,
        unsavedChanges: React.PropTypes.any,
        user: React.PropTypes.shape({
            name: React.PropTypes.string,
            auth: React.PropTypes.oneOf(["admin", "write", "read"]),
        }),
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
        this.props.dispatch(Actions.hamburgerVisibility(
            !this.props.showHamburger));
    },

    render: function() {
        const hamburgerContext = this.props.data.type === "collection" ?
                                 'collection' : 'item';
        return <div id="page">
            <Navbar
                cancelEditCallback={this.cancelEdits}
                data={this.props.data}
                editCallback={this.editToggle}
                editMode={this.props.editMode}
                onClickHamburger={this.toggleBurger}
            />
            {this.props.showHamburger ?
             <Hamburger
                 close={this.toggleBurger}
                 context={hamburgerContext}
                 getState={() => this.props.data}
                 user={this.props.user}
             /> : null}
            <div
                className={css(styles.pageContainerOuter)}
                onClick={this.props.showHamburger ? this.toggleBurger : null}
            >
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
