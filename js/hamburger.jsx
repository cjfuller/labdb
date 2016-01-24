const React = require("react");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const ss = require("./shared-styles.js");

const HamburgerEntry = React.createClass({
    propTypes: {
        children: React.PropTypes.node,
        iconName: React.PropTypes.string.isRequired,
        interactive: React.PropTypes.bool,
    },
    render: function() {
        return (
            <div
                className={
                    css(styles.hamburgerEntry,
                        this.props.interactive && styles.interactive)}
            >
                <div className={css(styles.icon)}>
                    <i className="material-icons">{this.props.iconName}</i>
                </div>
                {this.props.children}
            </div>
        );
    },
});

const HamburgerSectionName = React.createClass({
    propTypes: {
        name: React.PropTypes.string,
    },
    render: function() {
        return (
            <div className={css(styles.hamburgerSectionName)}>
                <span>{this.props.name}</span>
            </div>
        );
    },
});

const Hamburger = React.createClass({
    render: function() {
        return (
            <div className={css(styles.hamburgerMenu)}>
                <HamburgerSectionName name="Logged in as" />
                <HamburgerEntry iconName="person">
                    <span className={css(styles.userName)}>
                        Anonymous Person
                    </span>
                </HamburgerEntry>
                <HamburgerEntry iconName="cloud_off" interactive={true}>
                    <span>
                        Log out
                    </span>
                </HamburgerEntry>
                <HamburgerSectionName name="Administration" />
                <HamburgerEntry iconName="people" interactive={true}>
                    <span>Manage users</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="cloud_upload" interactive={true}>
                    <span>Bulk import data</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="cloud_download" interactive={true}>
                    <span>Bulk export data</span>
                </HamburgerEntry>
                <HamburgerSectionName name="Current item" />
                <HamburgerEntry iconName="content_copy" interactive={true}>
                    <span>Make a duplicate</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="file_download" interactive={true}>
                    <span>Export as JSON</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="delete" interactive={true}>
                    <span>Delete</span>
                </HamburgerEntry>
                <HamburgerSectionName name="All databases" />
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>Plasmids</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>Oligos</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>Bacteria</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>Samples</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>Antibodies</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>TC</span>
                </HamburgerEntry>
                <HamburgerEntry iconName="folder_open" interactive={true}>
                    <span>Yeast</span>
                </HamburgerEntry>
            </div>
        );
    },
});


const hamburgerBorderPx = 6;

const styles = StyleSheet.create({
    hamburgerEntry: {
        borderBottom: `1px solid ${ss.colors.borderColor}`,
        borderLeft: `${hamburgerBorderPx}px solid rgba(0,0,0,0)`,
        boxSizing: "border-box",
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-start",
        alignItems: "center",
        paddingLeft: ss.sizes.paddingPx,
        paddingRight: ss.sizes.paddingPx,
        width: "100%",
        height: "3.5em",
    },
    hamburgerSectionName: {
        display: "flex",
        alignItems: "flex-end",
        borderBottom: `1px solid ${ss.colors.borderColor}`,
        boxSizing: "borderBox",
        paddingLeft: ss.sizes.paddingPx,
        paddingRight: ss.sizes.paddingPx,
        width: "100%",
        height: "3.5em",
        fontFamily: ss.fonts.contrast,
        fontWeight: ss.fonts.weights.emph,
        verticalAlign: "baseline",
    },
    hamburgerMenu: {
        backgroundColor: "white",
        borderLeft: `4px solid ${ss.colors.labdbGreen}`,
        boxSizing: "border-box",
        fontFamily: ss.fonts.base,
        fontSize: ss.sizes.fontSizeMedium,
        position: "fixed",
        top: 0,
        height: "100vh",
        width: "300px",
        minWidth: "300px",
        overflowX: "hidden",
        overflowY: "auto",
        paddingTop: ss.sizes.navbarHeightPx,
        right: 0,
        zIndex: 10,
    },
    icon: {
        display: "inline-block",
        paddingTop: 2,
        margin: `0 ${ss.sizes.paddingPx}px`,
    },
    interactive: {
        ':hover': {
            borderLeft: (`${hamburgerBorderPx}px ` +
                         `solid ${ss.colors.mutedBlue}`),
        },
        cursor: "pointer",
    },
    userName: {
        fontFamily: ss.fonts.contrast,
        fontWeight: ss.fonts.weights.emph,
    },
});

module.exports = Hamburger;
