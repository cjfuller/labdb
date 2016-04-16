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
        return {searchTerms: '', includeSequence: false};
    },
    updateIncludeSequence: function() {
        this.setState({includeSequence: !this.state.includeSequence});
    },
    updateSearchState: function(value) {
        this.setState({searchTerms: value});
    },
    closeSearch: function() {
        this.props.dispatch(actions.searchVisibility(false));
    },
    doSearch: function() {
        return this.props.doSearch(this.state.searchTerms,
                                   this.state.includeSequence);
    },
    render: function() {
        return <div className={css(styles.searchBarOuter)}>
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
                <label className={css(styles.seqOption)}>
                    <input
                        type="checkbox"
                        checked={this.state.includeSequence}
                        className={css(styles.checkBox)}
                        value={this.state.includeSequence ? "true" : "false"}
                        onChange={this.updateIncludeSequence}
                    />
                    Include sequence in search?
                </label>
                <div
                    className={css(styles.searchButton)}
                    onClick={this.doSearch}
                >
                    Go
                </div>
            </div>
            <div className={css(styles.searchBarArrow)}>
            </div>
            <div
                className={css(styles.searchBarClose)}
                onClick={this.closeSearch}
            >
                <i className="material-icons">close</i>
            </div>
        </div>;
    },
});

const searchWrapMq = '@media(max-width: 899px)';

const styles = StyleSheet.create({
    searchBar: {
        alignItems: "center",
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-start",
        width: "100%",
        [searchWrapMq]: {
            alignItems: "flex-start",
            flexDirection: "column",
            justifyContent: "flex-start",
            marginTop: 10,
        },
    },
    searchBarOuter: {
        backgroundColor: "white",
        border: `1px solid ${ss.colors.borderColor}`,
        borderBottom: `1px solid ${ss.colors.borderColor}`,
        borderLeft: `1px solid ${ss.colors.borderColor}`,
        borderRadius: 4,
        display: "flex",
        fontFamily: ss.fonts.base,
        height: ss.sizes.navbarHeightPx,
        justifyContent: "space-between",
        paddingLeft: 10,
        position: "fixed",
        right: 0,
        top: ss.sizes.navbarHeightPx,
        width: "50vw",
        zIndex: 21,
        ...ss.traits.shadowed,
        [searchWrapMq]: {
            height: 2.5 * ss.sizes.navbarHeightPx,
        },
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
        ':hover': {
            cursor: "pointer",
        },
    },
    searchButton: {
        display: "flex",
        flexShrink: 0,
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
        flexShrink: 1,
        height: "1.2em",
        marginLeft: 10,
        minWidth: 175,
        width: "60%",
        [searchWrapMq]: {

            marginBottom: 10,
        },
    },
    seqOption: {
        flexShrink: 0,
        fontSize: ss.sizes.fontSizeCaption,
        maxHeight: ss.sizes.navbarHeightPx,
        [searchWrapMq]: {
            flexShrink: 1,
            marginLeft: 10,
            marginBottom: 10,
            overflow: "hidden",
        },
    },
});

module.exports = SearchBar;
