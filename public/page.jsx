window.Page = React.createClass({
    render: function() {
        return <div id="page">
            <Navbar data={this.props.data} />
            <div className="container">
                {this.props.data.type === "collection" ?
                    <ItemTable data={this.props.data} /> :
                    <ItemInfoView data={this.props.data} />}
            </div>
        </div>;
    },
});