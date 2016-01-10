const React = require("react");

const EditableText = require("./editable-text.jsx");

const {extVal} = require("./util.js");

const SupplementalField = React.createClass({
    propTypes: {
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        item: React.PropTypes.any, // TODO
        onChange: React.PropTypes.func,
    },
    render: function() {
        const f = this.props.item;
        const labelRef = "label-" + f.name;
        return <div className="supplemental-item">
            <div className="field-name suppl" ref={labelRef} key={labelRef}>
                {f.name + ":"}
            </div>
            <div
                aria-labelledby={this.refs.labelRef}
                className="field-value suppl"
                ref={"value-" + f.name}
            >
                <EditableText
                    value={extVal(this.props.data, f.lookup)}
                    editable={this.props.editable}
                    onChange={this.props.onChange}
                />
            </div>
        </div>;
    },
});

const SupplementalInfo = React.createClass({
    propTypes: {
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        makeUpdater: React.PropTypes.func,
        unsavedChanges: React.PropTypes.any, // TODO
    },
    getInitialState: function() {
        return {
            plasmidModalOpen: false,
        };
    },

    getFieldData: function() {
        if (this.props.editable) {
            return {...this.props.data.fieldData, ...this.props.unsavedChanges};
        }
        return this.props.data.fieldData;
    },

    render: function() {
        return <div className="three columns">
            <div className="supplemental-info">
            {this.props.data.supplementalFields.map((f) => {
                return <SupplementalField
                    data={this.getFieldData()}
                    editable={this.props.editable}
                    item={f}
                    key={f.name}
                    onChange={this.props.makeUpdater(f.lookup)}
                />;
            })}
            </div>
            {(this.props.data.type === "plasmid") ?
                <input
                    className="plasmap-button"
                    type="button"
                    value="Plasmid map"
                /> :
                null}
        </div>;
    },
});

module.exports = SupplementalInfo;
