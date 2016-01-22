const React = require("react");
const {StyleSheet, css} = require("../node_modules/aphrodite/lib/index.js");

const ActionExecutors = require("./action-executors.js");
const CoreInfo = require("./core-info.jsx");
const MegaBar = require("./megabar.jsx");
const ss = require("./shared-styles.js");
const SupplementalInfo = require("./supplemental-info.jsx");

const ItemInfoView = React.createClass({
    propTypes: {
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        unsavedChanges: React.PropTypes.any, // TODO
    },
    getInitialState: function() {
        return {itemUpdates: {}};
    },

    componentWillReceiveProps: function() {
        this.setState({itemUpdates: {}});
    },

    onUpdate: function(key, value) {
        ActionExecutors.editField(this.props.data.type,
                                  this.props.data.id,
                                  key,
                                  value);
    },

    makeUpdater: function(key) {
        return (value) => this.onUpdate(key, value);
    },

    render: function() {
        return <div className={css(styles.itemInfoView)}>
            <MegaBar
                data={this.props.data}
                editable={this.props.editable}
                unsavedChanges={this.props.unsavedChanges}
                makeUpdater={this.makeUpdater}
            />
            <div className={css(styles.infoRow)}>
                <div className={css(styles.coreInfoContainer)}>
                    <CoreInfo
                        data={this.props.data}
                        editable={this.props.editable}
                        unsavedChanges={this.props.unsavedChanges}
                        makeUpdater={this.makeUpdater}
                    />
                </div>
                <div className={css(styles.supplementalInfoContainer)}>
                    <SupplementalInfo
                        data={this.props.data}
                        editable={this.props.editable}
                        unsavedChanges={this.props.unsavedChanges}
                        makeUpdater={this.makeUpdater}
                    />
                </div>
            </div>
        </div>;
    },
});

const styles = StyleSheet.create({
    coreInfoContainer: {
        flex: 3,
    },
    itemInfoView: {
        fontFamily: ss.fonts.content,
        fontSize: ss.sizes.fontSizeMedium,
    },
    infoRow: {
        alignItems: "flex-start",
        display: "flex",
        flexDirection: "row",
        flexWrap: "wrap",
        marginBottom: 1.5 * ss.sizes.paddingPx,
    },
    supplementalInfoContainer: {
        flex: 1,
    },
});

module.exports = ItemInfoView;
