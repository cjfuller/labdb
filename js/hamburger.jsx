const React = require("react");
const {StyleSheet, css} = require("aphrodite");
const $ = require("jquery");

const ae = require("./action-executors.js");
const exportHandler = require("./export-handler.js");
const ss = require("./shared-styles.js");

const HamburgerEntry = React.createClass({
    propTypes: {
        children: React.PropTypes.node,
        href: React.PropTypes.string,
        iconName: React.PropTypes.string.isRequired,
        interactive: React.PropTypes.bool,
        onClick: React.PropTypes.func,
    },
    onClick: function() {
        if (this.props.href) {
            // TODO: make this not require a page reload.
            window.location = this.props.href;
        } else {
            this.props.onClick();
        }
    },
    render: function() {
        return (
            <div
                className={
                    css(styles.hamburgerEntry,
                        this.props.interactive && styles.interactive)}
                onClick={this.onClick}
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

function logout() {
    return $.ajax({
        url: '/logout',
        method: 'POST',
    }).then(() => {
        return window.gapi.auth2.getAuthInstance().signOut();
    }).then(() => {
        window.location.href = '/';
    });
}

const AuthScopes = ["admin", "write", "read"];
const AuthType = React.PropTypes.oneOf(AuthScopes);

/**
 * Control display of children based on auth scope.
 * (This is actually enforced server-side, and just so that we don't show
 * options that are forbidden anyway.)
 */
const Auth = React.createClass({
    propTypes: {
        auth: AuthType,
        children: React.PropTypes.node,
        required: AuthType,
    },
    meetsAuthRequirement: function() {
        return AuthScopes.indexOf(this.props.auth) <= AuthScopes.indexOf(this.props.required);
    },
    render: function() {
        if (this.meetsAuthRequirement()) {
            return <div>{this.props.children}</div>;
        }
        return null;
    },
});

const Hamburger = React.createClass({
    propTypes: {
        close: React.PropTypes.func,
        context: React.PropTypes.oneOf(['collection', 'item']),
        getState: React.PropTypes.func,
        user: React.PropTypes.shape({
            name: React.PropTypes.string,
            auth: AuthType,
        }),
    },
    promptUpload: function() {
        const input = document.createElement('input');
        input.type = 'file';
        input.multiple = true;
        input.onchange = this.doUpload(input);
        input.click();
    },
    doUpload: function(elt) {
        return () => {
            const data = new FormData();
            console.log(elt.files)
            const files = [];
            for (let i = 0; i < elt.files.length; i++) {
                files.push(elt.files.item(i));
            }
            files
               .filter((f) => f.type === 'application/json')
               .forEach((f, idx) => data.append(`file_${idx}`, f));
            $.ajax({
                url: '/api/v1/import',
                type: 'POST',
                data: data,
                processData: false,
                contentType: false,
            }).then(() => window.location.reload);
        };
    },
    render: function() {
        return (
            <div className={css(styles.hamburgerMenu)}>
                <HamburgerSectionName name="Logged in as" />
                <HamburgerEntry iconName="person">
                    <span className={css(styles.userName)}>
                        {this.props.user.name}
                    </span>
                </HamburgerEntry>
                <HamburgerEntry
                    iconName="cloud_off"
                    interactive={true}
                    onClick={logout}
                >
                    <span>
                        Log out
                    </span>
                </HamburgerEntry>
                <Auth
                    required="admin"
                    auth={this.props.user.auth}
                >
                    <HamburgerSectionName name="Administration" />
                    <HamburgerEntry
                        href="/users"
                        iconName="people"
                        interactive={true}
                    >
                        Manage users
                    </HamburgerEntry>
                    <HamburgerEntry
                        iconName="cloud_upload"
                        interactive={true}
                        onClick={this.promptUpload}
                    >
                        <span>Bulk import data</span>
                    </HamburgerEntry>
                    <HamburgerEntry iconName="cloud_download" interactive={false}>
                        <span>Bulk export data</span>
                        <span className={css(styles.comingSoon)}>
                            Coming soon!
                        </span>
                    </HamburgerEntry>
                </Auth>
                {this.props.context === 'item' ? <div>
                <HamburgerSectionName name="Current item" />
                <Auth
                    auth={this.props.user.auth}
                    required="write"
                >
                    <HamburgerEntry
                        iconName="content_copy"
                        interactive={true}
                        onClick={() => {
                            this.props.close();
                            ae.copyItem(this.props.getState());
                        }}
                    >
                        <span>Make a duplicate</span>
                    </HamburgerEntry>
                    {this.props.context === 'item' && this.props.getState().type === "plasmid" ?
                    <HamburgerEntry
                        iconName="library_add"
                        interactive={true}
                        onClick={() => {
                            this.props.close();
                            ae.strainFromPlasmid(this.props.getState());
                        }}
                    >
                        <span>Make a strain from this plasmid</span>
                    </HamburgerEntry> : null}
                </Auth>
                <HamburgerEntry
                    iconName="file_download"
                    interactive={true}
                    onClick={exportHandler(this.props.getState, 'json')}
                >
                    <span>Export as JSON</span>
                </HamburgerEntry>
                {this.props.getState().sequenceInfo ? <HamburgerEntry
                    iconName="file_download"
                    interactive={true}
                    onClick={exportHandler(this.props.getState, 'fasta')}
                >
                    <span>Export as FASTA</span>
                </HamburgerEntry> : null}
                <Auth
                    auth={this.props.user.auth}
                    required="write"
                >
                    <HamburgerEntry
                        iconName="delete"
                        interactive={true}
                        onClick={() => ae.deleteItem(this.props.getState())}
                    >
                        <span>Delete</span>
                    </HamburgerEntry>
                </Auth>
                </div> : null}
                <HamburgerSectionName name="All databases" />
                <HamburgerEntry
                    href="/plasmids"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>Plasmids</span>
                </HamburgerEntry>
                <HamburgerEntry
                    href="/oligos"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>Oligos</span>
                </HamburgerEntry>
                <HamburgerEntry
                    href="/bacteria"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>Bacteria</span>
                </HamburgerEntry>
                <HamburgerEntry
                    href="/samples"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>Samples</span>
                </HamburgerEntry>
                <HamburgerEntry
                    href="/antibodies"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>Antibodies</span>
                </HamburgerEntry>
                <HamburgerEntry
                    href="/lines"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>TC</span>
                </HamburgerEntry>
                <HamburgerEntry
                    href="/yeaststrains"
                    iconName="folder_open"
                    interactive={true}
                >
                    <span>Yeast</span>
                </HamburgerEntry>
            </div>
        );
    },
});


const hamburgerBorderPx = 6;

const styles = StyleSheet.create({
    comingSoon: {
        color: ss.colors.labdbGreen,
        fontSize: 10,
        marginLeft: 30,
    },
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
        ...ss.traits.shadowed,
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
