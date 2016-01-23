const React = require("react");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const ss = require("./shared-styles.js");

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
                className={css(styles.editableField)}
                disabled={!this.props.editable}
                onChange={(e) => this.props.onChange(e.target.value)}
                value={this.props.value}
            /> :
            <div>{this.props.value}</div>;
    }});

const styles = StyleSheet.create({
    editableField: {
        border: "none",
        fontFamily: ss.fonts.monospace,
        minHeight: 200,
        width: "100%",
    },
});

module.exports = EditableArea;
