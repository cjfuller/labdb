const {css, StyleSheet} = require("aphrodite");
const React = require("react");

const actions = require("./actions.js");
const ae = require("./action-executors.js");
const auth = require("./auth.js");
const ss = require("./shared-styles.js");

const NavItem = React.createClass({
    propTypes: {
        addr: React.PropTypes.string,
        name: React.PropTypes.string,
    },

    onClick: function() {
        // TODO: add to API and do this instead
        // window._store.updateDataFromURL(
        //     `${this.dynamicResourceBase}${this.props.addr}`);
        window.location.href = this.props.addr;
    },

    render: function() {
        return (
            <div
                className={css(styles.navitemContainer)}
                onClick={this.onClick}
            >
                <div
                    className={css(styles.navitem)}
                >
                    {this.props.name}
                </div>
            </div>
        );
    },

});

const NavLogo = React.createClass({
    propTypes: {
        text: React.PropTypes.string,
    },
    goHome: function() {
        window.location.pathname = "/";
    },
    render: function() {
        return <div
            className={css(styles.navlogo)}
            id="labdb"
            onClick={this.goHome}
        >
            {this.props.text}
        </div>;
    },
});

const CtxActions = React.createClass({
    propTypes: {
        cancelEditCallback: React.PropTypes.func,
        data: React.PropTypes.any,
        editCallback: React.PropTypes.func,
        editMode: React.PropTypes.bool,
    },
    collection: function() {
        return (this.props.data.type === "collection" ||
                this.props.data.type === "search");
    },
    doNextCollection: function() {
        // TODO: handle ascending sort
        const desc = true;
        let nextIndex = null;
        let sortOrder = null;
        // TODO: what if there's no items?  Should probably disable buttons.
        if (desc) {
            nextIndex = (
                this.props.data.items[this.props.data.items.length - 1]
                    .fieldData[this.props.data.numberFieldName] - 1);
            sortOrder = "DESC";
        } else {
            nextIndex = (
                this.props.data.items[this.props.data.items.length - 1]
                    .fieldData[this.props.data.numberFieldName] + 1);
            sortOrder = "ASC";
        }
        // TODO: make this not require a reload using:
        /* ae.maybeFetchThenDisplay("table", {
           type: this.props.data.items[0].type,
           start: nextIndex,
           }, sortOrder); */
        window.location.search = `sort_order=${sortOrder}&start=${nextIndex}`;
    },
    doNext: function() {
        // TODO: make this not require a reload.
        window.location = `${this.props.data.resourcePath}/next`;
    },

    doPrevious: function() {
        // TODO: make this not require a reload.
        window.location =  `${this.props.data.resourcePath}/previous`;
    },

    doPreviousCollection: function() {
        // TODO: handle ascending sort
        const desc = true;
        let nextIndex = null;
        let sortOrder = null;
        // TODO: is there a way to make this symmetric with the next
        // button in the face of missing items?
        const offset = 100;
        // TODO: what if there's no items?  Should probably disable buttons.
        if (desc) {
            nextIndex = (
                this.props.data.items[0]
                    .fieldData[this.props.data.numberFieldName] + offset);
            sortOrder = "DESC";
        } else {
            nextIndex = (
                this.props.data.items[0]
                    .fieldData[this.props.data.numberFieldName] - offset);
            sortOrder = "ASC";
        }
        window.location.search = `sort_order=${sortOrder}&start=${nextIndex}`;
    },
    render: function() {

        return <div className={css(styles.ctxactions)}>
            <div
                className={css(styles.action)}
                onClick={this.collection() ? this.doPreviousCollection :
                         this.doPrevious}
                title="previous"
            >
                <i className="material-icons">arrow_back</i>
            </div>
            <div
                className={css(styles.action)}
                onClick={this.collection() ? this.doNextCollection :
                         this.doNext}
                title="next"
            >
                <i className="material-icons">arrow_forward</i>
            </div>
            {this.props.editMode && auth('write') && !this.collection() ?
                <div
                    className={css(styles.action)}
                    onClick={this.props.cancelEditCallback}
                    title="discard changes"
                >
                    <i className="material-icons">block</i>
                </div> : null
            }
            {this.collection() || !auth('write') ? null :
            <div
                className={css(styles.action)}
                onClick={this.props.editCallback}
                title={this.props.editMode ? "save changes" : "edit"}
            >
                {this.props.editMode ?
                 <i className="material-icons">save</i> :
                 <i className="material-icons">mode_edit</i>}
            </div>}
        </div>;
    },
});

const Actions = React.createClass({
    // TODO: prop for whether to display new
    // TODO: correct path for search
    propTypes: {
        cancelEditCallback: React.PropTypes.func,
        data: React.PropTypes.any,
        dispatch: React.PropTypes.func,
        editCallback: React.PropTypes.func,
        editMode: React.PropTypes.bool,
        onClickHamburger: React.PropTypes.func,
    },

    newItem: function() {
        const {data} = this.props;
        const type = (data.type === "collection" ?
                      data.items.length && data.items[0].type :
                      data.type);
        // TODO: this won't work on a search with 0 items.
        // Probably should disable new on search.
        if (type) {
            ae.newItem(type);
        }
    },

    doSearch: function() {
        this.props.dispatch(actions.searchVisibility(true));
    },

    render: function() {
        return <div
            className={css(styles.actions)}
        >
            <CtxActions
                cancelEditCallback={this.props.cancelEditCallback}
                data={this.props.data}
                editCallback={this.props.editCallback}
                editMode={this.props.editMode}
            />
            <div className={css(styles.fixactions)}>
                {auth('write') && this.props.data['type'] !== 'search' ?
                 <div className={css(styles.action)} onClick={this.newItem}>
                     <i className="material-icons">add</i>
                 </div> : null}
                <div className={css(styles.action)} onClick={this.doSearch}>
                    <i className="material-icons">search</i>
                </div>
                <div className={css(styles.hamburgerWrapper)}>
                <div
                    className={css(styles.action, styles.hamburger)}
                    onClick={this.props.onClickHamburger}
                >
                    <i className="material-icons">menu</i>
                </div>
                </div>
            </div>
        </div>;
    },
});

const Navbar = React.createClass({
    propTypes: {
        cancelEditCallback: React.PropTypes.func,
        data: React.PropTypes.any,
        dispatch: React.PropTypes.func,
        editCallback: React.PropTypes.func,
        editMode: React.PropTypes.bool,
        navitems: React.PropTypes.arrayOf(React.PropTypes.string),
        navlinks: React.PropTypes.any,
        onClickHamburger: React.PropTypes.func,
    },

    getDefaultProps: function() {
        return {
            navitems: ["Plasmids", "Oligos", "Bacteria", "Samples",
                       "Antibodies", "TC", "Yeast"],
            navlinks: {
                Plasmids: "/plasmids",
                Oligos: "/oligos",
                Bacteria: "/bacteria",
                Samples: "/samples",
                Antibodies: "/antibodies",
                TC: "/lines",
                Yeast: "/yeaststrains",
            },
        };
    },

    render: function() {
        return <div className={css(styles.navbar)}>
            <div className={css(styles.navbarTextSection)}>
                <NavLogo text={"LabDB2.\u03b2"} />
                {this.props.navitems.map((n, i) => {
                    return <NavItem
                        addr={this.props.navlinks[n]}
                        name={n}
                        key={n}
                    />;
                })}
            </div>
            <Actions
                cancelEditCallback={this.props.cancelEditCallback}
                data={this.props.data}
                dispatch={this.props.dispatch}
                editCallback={this.props.editCallback}
                editMode={this.props.editMode}
                onClickHamburger={this.props.onClickHamburger}
            />
        </div>;
    },
});

const styles = StyleSheet.create({
    action: {
        alignItems: "center",
        display: "flex",
        ':hover': {
            cursor: "pointer",
        },
        justifyContent: "center",
        width: ss.sizes.hamburgerWidthPx,
    },
    actions: {
        alignContent: "center",
        display: "flex",
        flexDirection: "row",
        flexShrink: 0,
        height: ss.sizes.navbarHeightPx,
        justifyContent: "flex-end",
    },
    ctxactions: {
        alignItems: "center",
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-end",
    },
    fixactions: {
        alignItems: "center",
        display: "flex",
        flexDirection: "row",
    },
    hamburgerWrapper: {
        borderLeft: "1px solid black",
        boxSizing: "border-box",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        height: ss.sizes.navbarHeightPx,
        width: ss.sizes.hamburgerWidthPx,
    },
    navbar: {
        backgroundColor: ss.colors.labdbGreen,
        boxShadow: "0px 0px 4px 2px rgba(0, 0, 0, 0.2)",
        boxSizing: "border-box",
        display: "flex",
        fontFamily: ss.fonts.base,
        fontSize: ss.sizes.fontSizeLarge,
        height: ss.sizes.navbarHeightPx,
        justifyContent: "space-between",
        left: 0,
        position: "fixed",
        top: 0,
        width: "100vw",
        zIndex: 20,
    },
    navbarTextSection: {
        alignItems: "center",
        display: "flex",
        flexShrink: 1,
        marginLeft: ss.sizes.paddingPx,
        overflowX: "hidden",
    },
    navitem: {
        display: "inline-block",
        marginLeft: ss.sizes.paddingPx,
        marginRight: ss.sizes.paddingPx,
    },
    navitemContainer: {
        borderBottom: "3px solid rgba(0,0,0,0)",
        boxSizing: "border-box",
        display: "flex",
        flexDirection: "row",
        justifyContent: "center",
        alignItems: "center",
        ':hover': {
            cursor: "pointer",
            borderBottom: `3px solid ${ss.colors.mutedBlue}`,
        },
        height: ss.sizes.navbarHeightPx,
    },
    navlogo: {
        boxSizing: "border-box",
        fontFamily: "Rokkitt, serif",
        fontSize: "150%",
        paddingLeft: 2 * ss.sizes.paddingPx,
        paddingRight: 4 * ss.sizes.paddingPx,
    },
});

module.exports = Navbar;
