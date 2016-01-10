const React = require("react");

const EditableText = require("./editable-text.jsx");
const {extVal} = require("./util.js");

const MegaBar = React.createClass({
    propTypes: {
        data: React.PropTypes.shape({
            coreLinks: React.PropTypes.any,  // TODO
            fieldData: React.PropTypes.any,  // TODO
            name: React.PropTypes.string,
            shortDesc: React.PropTypes.any, // TODO
        }),
        editable: React.PropTypes.bool,
        makeUpdater: React.PropTypes.func,
        unsavedChanges: React.PropTypes.any, // TODO
    },
    getCoreLinks: function() {
        if (this.props.data.coreLinks) {
            return <div className="field-value">
                {this.props.editable ?
                    <EditableText
                        editable={this.props.editable}
                        onChange={this.props.makeUpdater(
                                      this.props.data.coreLinks.lookup)}
                        value={extVal(this.getFieldData(),
                                      this.props.data.coreLinks.lookup)}
                    /> :
                    this.props.data.coreLinks.links.map((link, i) => {
                        const [text, href] = link;
                        return <a href={href} key={href}>{text}</a>;
                    })
                }
            </div>;
        }
        return null;
    },

    getFieldData: function() {
        if (this.props.editable) {
            return {...this.props.data.fieldData, ...this.props.unsavedChanges};
        }
        return this.props.data.fieldData;
    },

    render: function() {
        const editingString = this.props.editable ? "Editing " : "";
        return <div className="item-megabar">
            <div className="item-id">
                {editingString + this.props.data.name}
            </div>
            <div className="row">
            <div className="nine columns">
            <div className="alias-field">
                {this.props.editable ?
                    <div className="field-name">
                        {this.props.data.shortDesc.name}
                    </div> :
                    null}
            <div className="field-value major">
                {this.props.editable ?
                    <EditableText
                        editable={this.props.editable}
                        onChange={this.props.makeUpdater(
                            this.props.data.shortDesc.lookup)}
                        value={extVal(
                            this.getFieldData(),
                            this.props.data.shortDesc.lookup)}
                    /> :
                    <div
                        className="item-alias"
                        dangerouslySetInnerHTML={{
                            __html: this.props.data.shortDesc.inlineValue,
                        }}
                    />}
            </div>
            </div>
            <div className="linked-items">
                {this.props.editable ?
                    <div className="field-name">
                        {(this.props.data.coreLinks || {}).name}
                    </div> :
                    null}
                {this.getCoreLinks()}
            </div>
            </div>
            <div className="three columns">
            </div>
            </div>

        </div>;
    },
});

module.exports = MegaBar;
