const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const actions = require("./actions.js");
const ss = require("./shared-styles.js");

const SearchBar = React.createClass({
    propTypes: {
        dispatch: React.PropTypes.func,
    },
    render: function() {
        return <div>
            <div className={css(styles.searchBar)}>
                I am the search bar.
                <div
                    className={css(styles.searchBarClose)}
                    onClick={() => this.props.dispatch(actions.searchVisibility(false))}
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
        borderBottom: `1px solid ${ss.colors.borderColor}`,
        borderLeft: `1px solid ${ss.colors.borderColor}`,
        display: "flex",
        flexDirection: "row",
        fontFamily: ss.fonts.base,
        height: ss.sizes.navbarHeightPx,
        justifyContent: "space-around",
        right: 0,
        position: "fixed",
        top: ss.sizes.navbarHeightPx,
        width: "50vw",
        zIndex: 21,
    },
    searchBarArrow: {
        position: "absolute",
        top: -16,
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
});

module.exports = SearchBar;
