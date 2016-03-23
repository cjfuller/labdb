const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const actions = require("./actions.js");
const EditableField = require("./editable-field.jsx");
const ss = require("./shared-styles.js");

const SearchBar = React.createClass({
    propTypes: {
        dispatch: React.PropTypes.func,
        doSearch: React.PropTypes.func,
    },
    getInitialState: function() {
        return {searchTerms: ''};
    },
    updateSearchState: function(value) {
        this.setState({searchTerms: value});
    },
    closeSearch: function() {
        this.props.dispatch(actions.searchVisibility(false));
    },
    doSearch: function() {
        return this.props.doSearch(this.state.searchTerms);
    },
    render: function() {
        return <div>
            <div className={css(styles.searchBar)}>
                <div>
                    Search:
                </div>
                <div className={css(styles.searchField)}>
                    <EditableField
                        autoFocus={true}
                        editable={true}
                        onChange={this.updateSearchState}
                        onEnter={this.doSearch}
                        value={this.state.searchTerms}
                    />
                </div>
                <div
                    className={css(styles.searchButton)}
                    onClick={this.doSearch}
                >
                    Go
                </div>
                <div
                    className={css(styles.searchBarClose)}
                    onClick={this.closeSearch}
                >
                    <i className="material-icons">close</i>
                </div>
                <div className={css(styles.searchBarArrow)}>
                </div>
            </div>
        </div>;
    },
});

const styles = StyleSheet.create({
    searchBar: {
        alignItems: "center",
        backgroundColor: "white",
        border: `1px solid ${ss.colors.borderColor}`,
        borderBottom: `1px solid ${ss.colors.borderColor}`,
        borderLeft: `1px solid ${ss.colors.borderColor}`,
        borderRadius: 4,
        display: "flex",
        flexDirection: "row",
        fontFamily: ss.fonts.base,
        height: ss.sizes.navbarHeightPx,
        justifyContent: "flex-start",
        right: 0,
        paddingLeft: 10,
        position: "fixed",
        top: ss.sizes.navbarHeightPx,
        width: "50vw",
        zIndex: 21,
        ...ss.traits.shadowed,
    },
    searchBarArrow: {
        position: "absolute",
        top: -15,
        right: 2.5 * ss.sizes.hamburgerWidthPx - 8,
        border: "8px solid rgba(0, 0, 0, 0)",
        borderBottom: "8px solid white",
        zIndex: 21,
    },
    searchBarClose: {
        position: "absolute",
        right: 0,
        top: 0,
        ':hover': {
            cursor: "pointer",
        },
    },
    searchButton: {
        display: "flex",
        border: `1px solid ${ss.colors.borderColor}`,
        alignItems: "center",
        justifyContent: "center",
        borderRadius: 3,
        height: "1.2em",
        marginLeft: 10,
        width: 50,
        ':hover': {
            cursor: "pointer",
        },
        ':active': {
            backgroundColor: ss.colors.bitDarkBackground,
        },
    },
    searchField: {
        height: "1.2em",
        marginLeft: 10,
        width: "60%",
    },
});

module.exports = SearchBar;
