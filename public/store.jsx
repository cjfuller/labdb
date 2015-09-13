var Data = window._labdbPrefetch;

var Store = React.createClass({
    componentDidMount: function() {
        var storeControl = {
            updateData: (newdata) => this.setState({data: newdata, push: true}),
            updateDataFromURL: (url) => {
                $.ajax({
                    url: url,
                    method: "GET",
                    success: (resp) => this.setState({data: resp, push: true}),
                });
            },
        };
        window._store = storeControl;
        window.onpopstate = (event) => {
            if (event.state) {
                this.setState({data: event.state, push: false});
            }
        }
        history.replaceState(this.state.data);
    },

    componentDidUpdate: function() {
        window.scroll(0, 0);
        if (this.state.data.resource && this.state.push) {
            history.pushState(
                this.state.data,
                null,
                this.state.data.resource);
        } else {

        }
    },

    getInitialState: function() {
        return {data: Data};
    },

    render: function() {
        return <Page data={this.state.data} />;
    },
});

React.render(<Store />, document.getElementById("body"));
