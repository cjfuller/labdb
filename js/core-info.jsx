const React = require("react");

const InfoSection = require("./info-section.jsx");
const InventoryWidget = require("./inventory-widget.jsx");
const SequenceSection = require("./sequence-section.jsx");
const {StyleSheet, css} = require("aphrodite");
const ss = require("./shared-styles.js");

const CoreInfo = React.createClass({
    propTypes: {
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        makeUpdater: React.PropTypes.func,
        unsavedChanges: React.PropTypes.any, // TODO
    },
    render: function() {
        return <div className={css(styles.coreInfo)}>
            {this.props.data.coreInfoSections.map((s) => {
                return <InfoSection
                    data={this.props.data.fieldData}
                    makeUpdater={this.props.makeUpdater}
                    editable={this.props.editable}
                    unsavedChanges={this.props.unsavedChanges}
                    key={s.name} name={s.name} contents={s}
                />;
            })}
            <SequenceSection
                data={this.props.data.fieldData}
                editable={this.props.editable}
                sequence={this.props.data.sequenceInfo}
                unsavedChanges={this.props.unsavedChanges}
                makeUpdater={this.props.makeUpdater}
            />
            <InventoryWidget inventory={this.props.data.inventory} />
        </div>;
    },
});

const styles = StyleSheet.create({
    coreInfo: {
        border: `1px solid ${ss.colors.borderColor}`,
        borderRadius: ss.sizes.cornerRadiusPx,
        margin: ss.sizes.paddingPx,
        padding: 1.5 * ss.sizes.paddingPx,
    },
});

module.exports = CoreInfo;
