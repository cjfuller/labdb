
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
    render: function () {
        return <div className="navitem" id="labdb">
            {this.props.text}
        </div>;
    },
});

var Separator = React.createClass({
    render: function () {
        return <div className="separator" />;
    },
});

var Hamburger = React.createClass({
    // TODO: prop for whether to display new
    // TODO: correct path for search

    newItem: function () {
        window.location.pathname += "/new";
    },

    doSearch: function () {
        window.location.pathname += "/search";
    },

    render: function () {
        return (
            <div className="fixactions">
                <div className="search">
                    <i className="material-icons" onClick={this.doSearch}>search</i>
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
            <NavLogo text={"LabDB2.\u03b2"} />
            <Separator />
            {_.map(this.props.navitems, function (n, i) {
                return <div>
                <NavItem name={n} addr={this.props.navlinks[n]}/>
                {i < this.props.navitems.length - 1 ? <Separator /> : null}
                </div>;
            }.bind(this))}
            <Hamburger />
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

React.render(
    <Navbar />,
    document.getElementById("navbar-top"));

React.render(
    <Subnav />,
    document.getElementById("subnav"));
