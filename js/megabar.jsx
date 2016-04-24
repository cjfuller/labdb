const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const EditableText = require("./editable-text.jsx");
const {extVal} = require("./util.js");
const ss = require("./shared-styles.js");

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
            return <div className={css(styles.fieldValue)}>
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
                        return <a
                            className={css(styles.coreLink)}
                            href={href}
                            key={text}
                        >
                            {text}
                        </a>;
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
        return <div className={css(styles.itemMegabar)}>
            <div className={css(styles.itemID)}>
                {editingString + this.props.data.name}
            </div>
            <div className={css(styles.subtitleContainer)}>
            <div className={css(styles.subtitle)}>
                <div className={css(styles.alias)}>
                    {this.props.editable ?
                        <div className={css(styles.fieldName)}>
                            {this.props.data.shortDesc.name}
                        </div> :
                        null}
                    <div className={css(styles.fieldValue)}>
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
                                    __html: (
                                        this.props.data.shortDesc.inlineValue),
                                }}
                            />}
                    </div>
                </div>
                <div className={css(styles.linkedItems)}>
                    {this.props.editable ?
                        <div className={css(styles.fieldName)}>
                            {(this.props.data.coreLinks || {}).name}
                        </div> :
                        null}
                    {this.getCoreLinks()}
                </div>
            </div>
            <div className={css(styles.placeholder)}>
            </div>
            </div>

        </div>;
    },
});

const styles = StyleSheet.create({
    alias: {
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-start",
        fontFamily: ss.fonts.monospace,
        fontSize: ss.sizes.fontSizeExtraLarge,
        minWidth: 400,
    },
    coreLink: {
        marginRight: ss.sizes.paddingPx,
        ...ss.elements.link,
    },
    fieldName: {
        whiteSpace: "nowrap",
        ...ss.elements.fieldName,
    },
    fieldValue: {
        display: "inline-block",
        width: "100%",
    },
    itemID: {
        fontSize: "3rem",
        fontFamily: ss.fonts.contrast,
    },
    itemMegabar: {
        margin: 2 * ss.sizes.paddingPx,
    },
    link: {
        ...ss.elements.link,
    },
    linkedItems: {
        display: "flex",
        flexDirection: "row",
        justifyContent: "flex-end",
        fontFamily: ss.fonts.base,
        fontSize: ss.sizes.fontSizeLarge,
    },
    placeholder: {
        flex: 1,
    },
    subtitle: {
        display: "flex",
        flex: 3,
        flexDirection: "row",
        justifyContent: "space-between",
    },
    subtitleContainer: {
        display: "flex",
        flexDirection: "row",
        justifyContent: "space-between",
    },
});

module.exports = MegaBar;
