const React = require("react");

const EditableArea = require("./editable-area.jsx");
const EditableField = require("./editable-field.jsx");

const EditableText = React.createClass({
    propTypes: {
        single: React.PropTypes.bool,
    },
    render: function() {
        return (this.props.single ?
                <EditableArea {...this.props} /> :
                <EditableField {...this.props} />);
    }});

module.exports = EditableText;
