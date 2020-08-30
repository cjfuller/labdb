import React from "react";
import _ from "underscore";
import {StyleSheet, css} from "aphrodite";

import * as Actions from "./actions"
import * as ActionExecutors from "./action-executors"
import Hamburger from "./hamburger"
import ItemTable from "./itemlist"
import ItemInfoView from "./itemview"
import Navbar from "./nav"
import PlasmidMap from "./plasmid-map"
import ss from "./shared-styles"
import SearchBar from "./search"

const Page = React.createClass({
    propTypes: {
        data: React.PropTypes.any,
        dispatch: React.PropTypes.func,
        editMode: React.PropTypes.bool,
        mapVisible: React.PropTypes.bool,
        showHamburger: React.PropTypes.bool,
        showSearch: React.PropTypes.bool,
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

    doSearch: function(searchTerm, includeSequence, person, types) {
        Actions.doSearchAndRedirect(searchTerm, includeSequence, person, types);
    },

    closePlasmidMap: function() {
        this.props.dispatch(Actions.mapVisibility(false));
    },

    render: function() {
        let hamburgerContext = null;
        let pageContent = null;
        switch (this.props.data.type) {
            case "search":
                hamburgerContext = "collection";
                if (!this.props.data.items) {
                    pageContent = <div className={css(styles.noResults)}>
                        No results found.
                    </div>;
                } else {
                    pageContent = (
                        <ItemTable
                            data={this.props.data}
                            dispatch={this.props.dispatch}
                            sort={["timestamp", "id"]}
                        />
                    );
                }
                break;
            case "collection":
                hamburgerContext = "collection";
                pageContent = (
                    <ItemTable
                        data={this.props.data}
                        dispatch={this.props.dispatch}
                    />
                );
                break;
            default:
                hamburgerContext = "item";
                pageContent = (
                    <ItemInfoView
                        data={this.props.data}
                        editable={this.props.editMode}
                        dispatch={this.props.dispatch}
                        unsavedChanges={this.props.unsavedChanges}
                    />
                );
        }
        const plasmidMapModal = this.props.mapVisible ?
                                <PlasmidMap
                                    data={this.props.data.plasmid_map}
                                    onClose={this.closePlasmidMap}
                                    plasmid={this.props.data}
                                /> : null;
        return <div id="page">
            <Navbar
                cancelEditCallback={this.cancelEdits}
                data={this.props.data}
                dispatch={this.props.dispatch}
                editCallback={this.editToggle}
                editMode={this.props.editMode}
                onClickHamburger={this.toggleBurger}
            />
            {this.props.showSearch ?
             <SearchBar
                 dispatch={this.props.dispatch}
                 doSearch={this.doSearch}
             /> : null}
            {this.props.showHamburger ?
             <Hamburger
                 close={this.toggleBurger}
                 context={hamburgerContext}
                 getState={() => this.props.data}
                 user={this.props.user}
             /> : null}
            {plasmidMapModal}
            <div
                className={css(styles.pageContainerOuter)}
                onClick={this.props.showHamburger ? this.toggleBurger : null}
            >
                <div className={css(styles.pageContainer)}>
                    {pageContent}
                </div>
            </div>
        </div>;
    },
});

const styles = StyleSheet.create({
    noResults: {
        marginTop: 10,
    },
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
