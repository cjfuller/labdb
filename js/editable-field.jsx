const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const ss = require("./shared-styles.js");

const EditableField = React.createClass({
    propTypes: {
        autoFocus: React.PropTypes.bool,
        editable: React.PropTypes.bool,
        fieldClasses: React.PropTypes.any, // TODO
        onChange: React.PropTypes.func,
        onEnter: React.PropTypes.func,
        value: React.PropTypes.node,
    },
    onKeyPress: function(event) {
        if (!this.props.onEnter) {
            return true;
        }
        if (event.charCode === 13) {
            this.props.onEnter();
            return false;
        }
        return true;
    },
    render: function() {
        {/* TODO: inject field classes */}
        return (
            <input
                autoFocus={!!this.props.autoFocus}
                className={
                    css(
                        this.props.editable && styles.editableField,
                        styles.field)}
                disabled={!this.props.editable}
                onChange={(e) => this.props.onChange(e.target.value)}
                onKeyPress={this.onKeyPress}
                ref="input"
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
