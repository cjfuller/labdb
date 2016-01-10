const React = require("react");

const EditableBoolean = require("./editable-boolean.jsx");
const EditableText = require("./editable-text.jsx");
const {extVal} = require("./util.js");

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
                <div className="field-name" ref={labelRef} key={labelRef}>
                    {f.name + sep}
                </div>
                <div
                    aria-labelledby={this.refs[labelRef]}
                    className={classNames({
                        core: true,
                        editable: this.props.editable,
                        'field-value': true,
                        major: this.props.contents.single})}
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
        return <div className="info-section">
            <div className="info-section-label" ref="sectionLabel">
                {this.props.name}
            </div>
            <div
                aria-labelledby={this.refs.sectionLabel}
                className="info-section-contents"
            >
                {this.getSectionContents()}
            </div>
        </div>;
    },

});

module.exports = InfoSection;
