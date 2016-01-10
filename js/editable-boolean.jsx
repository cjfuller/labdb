const React = require("react");

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
        return <div className="boolean-editor">
            <div
                className={classNames({"boolean-editor-yes": true,
                                       selected: this.props.value === true})}
                onClick={this.setValue(true)}
            >
                {"\u2714"}
            </div>
            <div
                className={classNames({"boolean-editor-no": true,
                                       selected: this.props.value === false})}
                onClick={this.setValue(false)}
            >
                {"\u2716"}
            </div>
            <div
                className={classNames({
                    "boolean-editor-unknown": true,
                    selected: (this.props.value === null ||
                               this.props.value === undefined),
                })}
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

module.exports = EditableBoolean;
