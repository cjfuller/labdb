const React = require("react");

// TODO: replace with inline styles.
const classNames = () => "";

const EditableField = React.createClass({
    propTypes: {
        editable: React.PropTypes.bool,
        fieldClasses: React.PropTypes.any, // TODO
        onChange: React.PropTypes.func,
        value: React.PropTypes.node,
    },
    render: function() {
        return <input
            className={classNames({
                editableField: true,
                editable: this.props.editable,
            }, this.props.fieldClasses)}
            disabled={!this.props.editable}
            onChange={(e) => this.props.onChange(e.target.value)}
            type="text"
            value={this.props.value}
        />;
    },
});

module.exports = EditableField;
