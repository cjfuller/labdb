const React = require("react");

// TODO: replace with inline styles.
const classNames = () => "";

const EditableArea = React.createClass({
    propTypes: {
        editable: React.PropTypes.bool,
        fieldClasses: React.PropTypes.any, // TODO
        onChange: React.PropTypes.func,
        value: React.PropTypes.string,
    },
    render: function() {
        return this.props.editable ?
            <textarea
                className={classNames({
                    editableField: true,
                    editable: this.props.editable,
                }, this.props.fieldClasses)}
                disabled={!this.props.editable}
                onChange={(e) => this.props.onChange(e.target.value)}
                value={this.props.value}
            /> :
            <div>{this.props.value}</div>;
    }});

module.exports = EditableArea;
