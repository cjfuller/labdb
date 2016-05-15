const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const actions = require("./actions.js");
const EditableField = require("./editable-field.jsx");
const ss = require("./shared-styles.js");

// TODO: determine dynamically; share code with nav
const Types = {
    Plasmid: "Plasmids",
    Oligo: "Oligos",
    Bacterium: "Bacteria",
    Sample: "Samples",
    Antibody: "Antibodies",
    Line: "TC",
    Yeaststrain: "Yeast",
};

const TypeSelector = React.createClass({
    propTypes: {
        onChange: React.PropTypes.func,
        selected: React.PropTypes.arrayOf(React.PropTypes.string),
    },
    render: function() {
        const typeSelectors = Object.keys(Types).map((k) => {
            return <div
                key={k}
                onClick={() => this.props.onChange(k)}
                className={css(styles.typeSelector,
                               this.props.selected.includes(k) && styles.typeSelectorSelected)}
            >
                {Types[k]}
            </div>
        });
        return <div className={css(styles.typeSelectorOuter)}>
            {typeSelectors}
        </div>
    },
});

const SearchBar = React.createClass({
    propTypes: {
        dispatch: React.PropTypes.func,
        doSearch: React.PropTypes.func,
    },
    getInitialState: function() {
        return {
            searchTerms: '',
            includeSequence: false,
            person: null,
            types: Object.keys(Types),
        };
    },
    updateIncludeSequence: function() {
        this.setState({includeSequence: !this.state.includeSequence});
    },
    updateSearchState: function(value) {
        this.setState({searchTerms: value});
    },
    updatePersonField: function(value) {
        this.setState({person: value});
    },
    updateTypes: function(type) {
        if (this.state.types.includes(type)) {
            this.setState({types: this.state.types.filter((t) => t !== type)});
        } else {
            this.setState({types: this.state.types.concat(t)});
        }
    },
    closeSearch: function() {
        this.props.dispatch(actions.searchVisibility(false));
    },
    doSearch: function() {
        return this.props.doSearch(this.state.searchTerms,
                                   this.state.includeSequence,
                                   this.state.person,
                                   this.state.types);
    },
    render: function() {
        return <div className={css(styles.searchBarOuter)}>
            <div className={css(styles.searchBar)}>
                <div className={css(styles.searchBarRow)}>
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
                </div>
                <div className={css(styles.searchBarRow)}>
                    <label>
                        <div className={css(styles.inline)}>
                            Person:
                        </div>
                        <EditableField
                            autoFocus={false}
                            editable={true}
                            extraStyles={[styles.inline, styles.searchField]}
                            onChange={this.updatePersonField}
                            value={this.state.person}
                        />
                    </label>
                </div>
                <div className={css(styles.searchBarRow)}>
                    <div className={css(styles.inline)}>
                        Types to search:
                        <TypeSelector
                            onChange={this.updateTypes}
                            selected={this.state.types}
                        />
                    </div>
                </div>
                <div className={css(styles.searchBarRow)}>
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
    inline: {
        display: "inline-block",
    },
    searchBar: {
        alignItems: "flex-start",
        display: "flex",
        flexDirection: "column",
        justifyContent: "flex-start",
        width: "100%",
    },
    searchBarRow: {
        alignItems: "center",
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-start",
        marginBottom: 10,
        marginTop: 10,
        whiteSpace: "nowrap",
        width: "100%",
    },
    searchBarOuter: {
        backgroundColor: "white",
        border: `1px solid ${ss.colors.borderColor}`,
        borderBottom: `1px solid ${ss.colors.borderColor}`,
        borderLeft: `1px solid ${ss.colors.borderColor}`,
        borderRadius: 4,
        display: "flex",
        fontFamily: ss.fonts.base,
        justifyContent: "space-between",
        paddingLeft: 10,
        position: "fixed",
        right: 0,
        top: ss.sizes.navbarHeightPx,
        width: "30vw",
        zIndex: 21,
        ...ss.traits.shadowed,
    },
    searchBarArrow: {
        position: "absolute",
        top: -15,
        right: 1.5 * ss.sizes.hamburgerWidthPx - 8,
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
        width: "80%",
    },
    seqOption: {
        flexShrink: 0,
        fontSize: ss.sizes.fontSizeCaption,
        maxHeight: ss.sizes.navbarHeightPx,
    },
    typeSelectorOuter: {
        display: "inline-flex",
        alignItems: "center",
        justifyContent: "flex-start",
        marginLeft: 10,
    },
    typeSelector: {
        boxSizing: "border-box",
        border: `1px solid ${ss.colors.borderColor}`,
        fontSize: ss.sizes.fontSizeCaption,
        padding: 5,
        ':not(:last-of-type)': {
            borderRight: "none",
        },
        ':hover': {
            cursor: "pointer",
            borderBottom: `1px solid ${ss.colors.mutedBlueMoreOpaque}`,
        },
    },
    typeSelectorSelected: {
        backgroundColor: ss.colors.mutedBlueSemitransparent,
    },
});

module.exports = SearchBar;
