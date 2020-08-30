import React from "react"
import {StyleSheet, css} from "aphrodite"

import * as ActionExecutors from "./action-executors"
import CoreInfo from "./core-info"
import MegaBar from "./megabar"
import ss from "./shared-styles"
import SupplementalInfo from "./supplemental-info"

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

const smallScreen = '@media(max-width: 899px)';

const styles = StyleSheet.create({
    coreInfoContainer: {
        flexGrow: 3,
        flexShrink: 3,
        maxWidth: "85%",
        [smallScreen]: {
            maxWidth: "100%",
        },
    },
    itemInfoView: {
        fontFamily: ss.fonts.content,
        fontSize: ss.sizes.fontSizeMedium,
    },
    infoRow: {
        alignItems: "flex-start",
        display: "flex",
        flexDirection: "row",
        marginBottom: 1.5 * ss.sizes.paddingPx,
        maxWidth: "100%",
        [smallScreen]: {
            flexDirection: "column",
        },
    },
    supplementalInfoContainer: {
        flexGrow: 1,
        flexShrink: 1,
    },
});

module.exports = ItemInfoView;
