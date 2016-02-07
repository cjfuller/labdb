const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const ss = require("./shared-styles.js");

// TODO: replace with inline styles.
const classNames = () => "";

const BooleanEditingControl = React.createClass({
    propTypes: {
        onChange: React.PropTypes.func,
        value: React.PropTypes.bool,
    },
    setValue: function(val) {
        return (e) => this.props.onChange(val);
    },

    render: function() {
        return <div className={css(styles.booleanEditor)}>
            <div
                className={css(
                        styles.booleanEditorOption,
                        (this.props.value === true) && styles.selectedYes)}
                onClick={this.setValue(true)}
            >
                {"\u2714"}
            </div>
            <div
                className={css(
                        styles.booleanEditorOption,
                        (this.props.value === false) && styles.selectedNo)}
                onClick={this.setValue(false)}
            >
                {"\u2716"}
            </div>
            <div
                className={css(
                        styles.booleanEditorOption,
                        (this.props.value === null ||
                         this.props.value === undefined) &&
                        styles.selectedUnknown)}
                onClick={this.setValue(null)}
            >
                ?
            </div>
        </div>;
    },
});


const EditableBoolean = React.createClass({
    propTypes: {
        editable: React.PropTypes.bool,
        unknownMarker: React.PropTypes.string,
        value: React.PropTypes.bool,
    },
    getDefaultProps: function() {
        return {unknownMarker: ""};
    },
    getIconValue: function(v) {
        if (v === true) {
            return <div className="yes-icon">
                <i className="material-icons">check</i>
            </div>;
        } else if (v === false) {
            return <div className="no-icon">
                <i className="material-icons">close</i>
            </div>;
        } else {
            return <div className="unknown-icon">
                {this.props.unknownMarker}
            </div>;
        }
    },

    render: function() {
        return this.props.editable ?
            <BooleanEditingControl {...this.props} /> :
            this.getIconValue(this.props.value);
    },
});

const styles = StyleSheet.create({
    booleanEditor: {
        display: "inline-block",
        border: `1px solid ${ss.colors.bitDarkBackground}`,
        borderRadius: ss.sizes.cornerRadiusPx,
        marginTop: ss.sizes.paddingPx / 2,
        marginBottom: ss.sizes.paddingPx / 2,
        ...ss.traits.shadowedButton,
    },
    booleanEditorOption: {
        display: "inline-block",
        minWidth: 30,
        textAlign: "center",
        ':hover': {
            backgroundColor: ss.colors.borderColor,
            cursor: "pointer",
        },
    },
    selectedYes: {
        backgroundColor: ss.colors.yesGreen,
        ':hover': {
            backgroundColor: ss.colors.yesGreen,
        },
    },
    selectedNo: {
        backgroundColor: ss.colors.noRed,
        ':hover': {
            backgroundColor: ss.colors.noRed,
        },
    },
    selectedUnknown: {
        backgroundColor: ss.colors.ambiguousBlue,
        ':hover': {
            backgroundColor: ss.colors.ambiguousBlue,
        },
    },
});

module.exports = EditableBoolean;
