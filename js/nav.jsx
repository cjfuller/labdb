const {css, StyleSheet} = require("../node_modules/aphrodite/lib/index.js");
const React = require("react");
const _ = require("underscore");
const $ = require("jquery");

const ss = require("./shared-styles.js");

const NavItem = React.createClass({
    propTypes: {
        addr: React.PropTypes.string,
        name: React.PropTypes.string,
    },

    // TODO: get this from somewhere instead of hardcoding
    dynamicResourceBase: "/api/v1/m",

    onClick: function() {
        // TODO: add to API and do this instead
        // window._store.updateDataFromURL(
        //     `${this.dynamicResourceBase}${this.props.addr}`);
        window.location.pathname = this.props.addr;
    },

    render: function() {
        return <div className={css(styles.navitem)} onClick={this.onClick}>
            {this.props.name}
        </div>;
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
    doNext: function() {
        const apiBase = this.props.data.dynamicResourceBase;
        const resource = this.props.data.resource;
        $.ajax({
            url: `${apiBase}${resource}/next`,
            method: "GET",
            success: (resp) => window._store.updateDataFromURL(
                `${resp}`),
        });
    },

    doPrevious: function() {
        const apiBase = this.props.data.dynamicResourceBase;
        const resource = this.props.data.resource;
        $.ajax({
            url: `${apiBase}${resource}/previous`,
            method: "GET",
            success: (resp) => window._store.updateDataFromURL(
                `${resp}`),
        });
    },

    render: function() {
        if (_.isArray(window._labdbPrefetch)) {
            return null;
        }

        return <div className={css(styles.ctxactions)}>
            <div
                className={css(styles.action)}
                onClick={this.doPrevious}
                title="previous"
            >
                <i className="material-icons">arrow_back</i>
            </div>
            <div
                className={css(styles.action)}
                onClick={this.doNext}
                title="next"
            >
                <i className="material-icons">arrow_forward</i>
            </div>
            {this.props.editMode ?
                <div
                    className={css(styles.action)}
                    onClick={this.props.cancelEditCallback}
                    title="discard changes"
                >
                    <i className="material-icons">block</i>
                </div> : null
            }
            <div
                className={css(styles.action)}
                onClick={this.props.editCallback}
                title={this.props.editMode ? "save changes" : "edit"}
            >
                {this.props.editMode ?
                 <i className="material-icons">save</i> :
                 <i className="material-icons">mode_edit</i>}
            </div>
        </div>;
    },
});

const Actions = React.createClass({
    // TODO: prop for whether to display new
    // TODO: correct path for search
    propTypes: {
        cancelEditCallback: React.PropTypes.func,
        data: React.PropTypes.any,
        editCallback: React.PropTypes.func,
        editMode: React.PropTypes.bool,
        onClickHamburger: React.PropTypes.func,
    },

    newItem: function() {
        // TODO: real implementation
        window.location.pathname += "/new";
    },

    doSearch: function() {
        // TODO: real implementation
        window.location.pathname += "/search";
    },

    render: function() {
        return <div className={css(styles.actions)}>
            <CtxActions
                cancelEditCallback={this.props.cancelEditCallback}
                data={this.props.data}
                editCallback={this.props.editCallback}
                editMode={this.props.editMode}
            />
            <div className={css(styles.fixactions)}>
                <div className={css(styles.action)} onClick={this.doSearch}>
                    <i className="material-icons">search</i>
                </div>
                <div className={css(styles.action)} onClick={this.newItem}>
                   <i className="material-icons">add</i>
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
                {_.map(this.props.navitems, function(n, i) {
                    return <NavItem
                        addr={this.props.navlinks[n]}
                        name={n}
                        key={n}
                    />;
                }.bind(this))}
            </div>
            <Actions
                cancelEditCallback={this.props.cancelEditCallback}
                data={this.props.data}
                editCallback={this.props.editCallback}
                editMode={this.props.editMode}
                onClickHamburger={this.props.onClickHamburger}
            />
        </div>;
    },
});

const styles = StyleSheet.create({
    action: {
        display: "inline-block",
        ':hover': {
            cursor: "pointer",
        },
        marginLeft: ss.sizes.paddingPx,
        marginRight: ss.sizes.paddingPx,
    },
    actions: {
        alignContent: "center",
        display: "flex",
        flexDirection: "row",
        height: ss.sizes.navbarHeightPx,
        justifyContent: "flex-end",
        marginRight: ss.sizes.paddingPx,
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
        height: ss.sizes.navbarHeightPx,
        paddingLeft: ss.sizes.paddingPx / 2,
        paddingRight: ss.sizes.paddingPx,
    },
    navbar: {
        backgroundColor: ss.colors.labdbGreen,
        display: "flex",
        fontFamily: ss.fonts.base,
        height: ss.sizes.navbarHeightPx,
        justifyContent: "space-between",
        left: 0,
        position: "fixed",
        top: 0,
        width: "100vw",
    },
    navbarTextSection: {
        alignItems: "center",
        display: "flex",
        marginLeft: ss.sizes.paddingPx,
    },
    navitem: {
        display: "inline-block",
        ':hover': {
            cursor: "pointer",
        },
        marginLeft: ss.sizes.paddingPx,
        marginRight: ss.sizes.paddingPx,
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