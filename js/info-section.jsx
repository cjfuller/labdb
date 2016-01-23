const React = require("react");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const EditableBoolean = require("./editable-boolean.jsx");
const EditableText = require("./editable-text.jsx");
const {extVal} = require("./util.js");
const ss = require("./shared-styles.js");

// TODO: replace with inline styles
const classNames = () => "";

const InfoSection = React.createClass({
    propTypes: {
        contents: React.PropTypes.any, // TODO
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        makeUpdater: React.PropTypes.func,
        name: React.PropTypes.string,
        unsavedChanges: React.PropTypes.any, // TODO
    },
    val: function(l) {
        return extVal({...this.props.data, ...this.props.unsavedChanges}, l);
    },
    getSectionContents: function() {
        if (this.props.contents.preformatted) {
            return this.props.editable ?
                <div
                    className={classNames({
                        "field-value": true,
                        core: true,
                        editable: this.props.editable,
                        major: this.props.contents.single})
                    }
                    key="editable"
                >
                    <EditableText
                        value={this.val(this.props.contents.lookup)}
                        editable={this.props.editable}
                        single={this.props.contents.single}
                        onChange={this.props.makeUpdater(
                            this.props.contents.lookup)}
                    />
                </div> :
                <div
                    className="field-value"
                    dangerouslySetInnerHTML={{
                        __html: this.props.contents.inlineValue,
                    }}
                    key="prerendered"
                />;
        }
        return this.props.contents.fields.map((f) => {
            const isBooleanField = f.type && f.type === "boolean";
            const sep = isBooleanField ? "?" : ":";

            const labelRef = "label-" + f.name;
            return <div key={f.name}>
                <div
                    className={css(styles.fieldName)}
                    key={labelRef}
                    ref={labelRef}
                >
                    {f.name + sep}
                </div>
                <div
                    aria-labelledby={this.refs[labelRef]}
                    className={css(
                            this.props.editable && styles.editableField,
                            styles.fieldValue,
                            this.props.contents.single && styles.major)}
                    key={"value-" + f.name}
                >
                    {isBooleanField ?
                     <EditableBoolean
                         editable={this.props.editable}
                         onChange={this.props.makeUpdater(f.lookup)}
                         unknownMarker="?"
                         value={this.val(f.lookup)}
                     /> :
                     <EditableText
                         editable={this.props.editable}
                         onChange={this.props.makeUpdater(f.lookup)}
                         value={this.val(f.lookup)}
                     />}
                </div>
            </div>;
        });
    },

    render: function() {
        return <div className={css(styles.infoSection)}>
            <div className={css(styles.infoSectionLabel)} ref="sectionLabel">
                {this.props.name}
            </div>
            <div
                aria-labelledby={this.refs.sectionLabel}
                className={css(styles.infoSectionContents)}
            >
                {this.getSectionContents()}
            </div>
        </div>;
    },

});

const styles = StyleSheet.create({
    fieldName: {
        ...ss.elements.fieldName,
    },
    infoSection: {
        ':not(:last-of-type)': {
            marginBottom: 2 * ss.sizes.paddingPx,
        },
    },
    infoSectionContents: {
        ...ss.elements.sectionContents,
    },
    infoSectionLabel: {
        ...ss.elements.sectionLabel,
    },
    editableField: {
        ...ss.traits.editableBorders,
        ...ss.traits.editableFocus,
    },
    major: {
        maxWidth: "100%",
        width: "100%",
    },
    fieldValue: {
        display: "inline-block",
    },
});


module.exports = InfoSection;
