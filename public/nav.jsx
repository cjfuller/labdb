
var NavItem = React.createClass({

    propTypes: {

    },

    onClick: function () {
        window.location.pathname = this.props.addr;
    },

    render: function () {
        return <div className="navitem" onClick={this.onClick}>
            {this.props.name}
        </div>
    },

});

var NavLogo = React.createClass({
    goHome: function () {
        window.location.pathname = "/";
    },
    render: function () {
        return <div className="navlogo" id="labdb" onClick={this.goHome}>
            {this.props.text}
        </div>;
    },
});

var CtxActions = React.createClass({
    doNext: function() {
        var apiBase = this.props.data.dynamicResourceBase;
        var resource = this.props.data.resource;
        var itemType = this.props.data.type;
        var itemId = this.props.data.id;
        $.ajax({
            url: `${apiBase}${resource}/next`,
            method: "GET",
            success: (resp) => window._store.updateDataFromURL(
                `${resp}`),
        });
    },

    doPrevious: function() {
        var apiBase = this.props.data.dynamicResourceBase;
        var resource = this.props.data.resource;
        var itemType = this.props.data.type;
        var itemId = this.props.data.id;
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

        return <div className="ctxactions">
            <div className="previous-item" onClick={this.doPrevious}>
                <i className="material-icons">arrow_back</i>
            </div>
            <div className="next-item" onClick={this.doNext}>
                <i className="material-icons">arrow_forward</i>
            </div>
        </div>;
    },
});

var Actions = React.createClass({
    // TODO: prop for whether to display new
    // TODO: correct path for search

    newItem: function () {
        // TODO: real implementation
        window.location.pathname += "/new";
    },

    doSearch: function () {
        // TODO: real implementation
        window.location.pathname += "/search";
    },

    render: function () {
        return <div className="actions">
            <CtxActions data={this.props.data} />
            <div className="fixactions">
                <div className="search" onClick={this.doSearch}>
                    <i className="material-icons">search</i>
                </div>
                <div className="new-item" onClick={this.newItem}>
                   <i className="material-icons">add</i>
                </div>
                <div className="hamburger">
                    <i className="material-icons">menu</i>
                </div>
            </div>
        </div>;
    },
});


window.Navbar = React.createClass({

    getDefaultProps: function() {
        return {
            navitems: ["Plasmids", "Oligos", "Bacteria", "Samples", "Antibodies", "TC", "Yeast"],
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

    propTypes: {
        navitems: React.PropTypes.arrayOf(React.PropTypes.string),
    },


    render: function() {
        return <div className="navbar-top">
            <div className="navbar-variable">
            <NavLogo text={"LabDB2.\u03b2"} />
            {_.map(this.props.navitems, function (n, i) {
                return <NavItem name={n} addr={this.props.navlinks[n]}/>;
            }.bind(this))}
            </div>
            <Actions data={this.props.data}/>
        </div>;
    },
});
