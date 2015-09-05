
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
    render: function() {
        return null;
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

    doNext: function() {
        // TODO: ugh, fix this.
        var item_id = window.location.pathname.split("/")[-1];
        window.location.pathname += "/next." + item_id;
    },

    doPrevious: function() {
        // TODO: ugh, fix this.
        var item_id = window.location.pathname.split("/")[-1];
        window.location.pathname += "/previous." + item_id;
    },

    render: function () {
        return (
            <div className="fixactions">
                <div className="previous-item" onClick={this.doPrevious}>
                    <i className="material-icons">arrow_back</i>
                </div>
                <div className="next-item" onClick={this.doNext}>
                    <i className="material-icons">arrow_forward</i>
                </div>
                <div className="search" onClick={this.doSearch}>
                    <i className="material-icons">search</i>
                </div>
                <div className="new-item" onClick={this.newItem}>
                   <i className="material-icons">add</i>
                </div>
                <div className="hamburger">
                    <i className="material-icons">menu</i>
                </div>
            </div>);
    },
});


var Navbar = React.createClass({

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
            <Actions />
        </div>;
    },
});

var Subnav = React.createClass({
    render: function() {
        return <div className="subnav">
        <NavItem name="Iteminfo here." />

        </div>;
    },

});

var navbar = document.getElementById("navbar-top");
if (navbar){
    React.render(<Navbar />, navbar);
}

var subnav = document.getElementById("subnav");
if (subnav) {
    React.render(<Subnav />, subnav);
}
