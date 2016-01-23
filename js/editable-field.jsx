const React = require("react");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const ss = require("./shared-styles.js");

const EditableField = React.createClass({
    propTypes: {
        editable: React.PropTypes.bool,
        fieldClasses: React.PropTypes.any, // TODO
        onChange: React.PropTypes.func,
        value: React.PropTypes.node,
    },
    render: function() {
        {/* TODO: inject field classes */}
        return (
            <input
                className={
                    css(
                        this.props.editable && styles.editableField,
                        styles.field)}
                disabled={!this.props.editable}
                onChange={(e) => this.props.onChange(e.target.value)}
                type="text"
                value={this.props.value}
            />);
    },
});

const styles = StyleSheet.create({
    editableField: {
        ...ss.traits.editableBorders,
        ...ss.traits.editableFocus,
    },
    field: {
        ...ss.elements.inputField,
    },
});

module.exports = EditableField;
