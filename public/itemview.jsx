console.log(document.getElementById("item-data").dataset);

var ItemData = document.getElementById("item-data").dataset;

console.log(ItemData);

var MegaBar = React.createClass({
    render: function() {
        return <div className="item-megabar">
        <div className="item-id">
        <h3>
            {ItemData.name}
        </h3>
        </div>
        <div className="linked-items"/>
        </div>;
    },
});

var CoreInfo = React.createClass({
    render: function() {
        return <div className="core-info">
        <div className="field-name">
        Alias
        </div>
        <div className="field-value editable">
            <div className="short-desc" dangerouslySetInnerHTML={{__html: ItemData.shortDesc}} />
        </div>
        </div>;
    },
});


var ItemInfoView = React.createClass({
    render: function() {
        return <div className="item-info-view">
        I am the item info view.
        <MegaBar />
        <CoreInfo />
        </div>;
    },
});

React.render(<ItemInfoView />, document.getElementById("item-info"));