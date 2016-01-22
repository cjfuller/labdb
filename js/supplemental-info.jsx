const React = require("react");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const EditableText = require("./editable-text.jsx");
const ss = require("./shared-styles.js");

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
        return <div className={css(styles.supplementWrapper)}>
            <div className={css(styles.supplementalInfo)}>
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
                <div
                    className={css(styles.plasmapButton)}
                    role="button"
                >
                    Plasmid map
                </div> :
                null}
        </div>;
    },
});

const styles = StyleSheet.create({
    plasmapButton: {
        alignItems: "center",
        border: `1px solid ${ss.colors.borderColor}`,
        borderRadius: ss.sizes.cornerRadiusPx,
        boxShadow: `1px 1px 1px ${ss.colors.borderColor}`,
        boxSizing: "border-box",
        display: "flex",
        height: ss.sizes.buttonHeightPx,
        justifyContent: "center",
        margin: ss.sizes.paddingPx,
        textAlign: "center",
        width: "100%",
        ':active': {
            backgroundColor: ss.colors.mediumBackground,
            boxShadow: `inset 1px 1px ${ss.colors.borderColor}`,
        },
        ':hover': {
            backgroundColor: ss.colors.lightBackground,
            cursor: "pointer",
        },
    },
    supplementalInfo: {
        border: `1px solid ${ss.colors.borderColor}`,
        borderRadius: ss.sizes.cornerRadiusPx,
        boxSizing: "border-box",
        margin: ss.sizes.paddingPx,
        padding: 1.5 * ss.sizes.paddingPx,
        width: "100%",
    },
    supplementWrapper: {
        boxSizing: "border-box",
        display: "block",
        width: "100%",
    },
});

module.exports = SupplementalInfo;