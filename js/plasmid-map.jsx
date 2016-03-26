const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const Modal = require("./modal.jsx");
const showMap = require("./plasmid-map.js");
const ss = require("./shared-styles.js");

const PlasmidMap = React.createClass({
    propTypes: {
        data: React.PropTypes.any,
        onClose: React.PropTypes.func,
    },
    componentDidMount: function() {
        showMap();
    },
    render: function() {
        // TODO: reactify!
        return <Modal onClose={this.props.onClose}>
            <div className={css(styles.modalContent)}>
                <div
                    className={css(styles.plasmidMap)}
                    data={JSON.stringify(this.props.data)}
                    id="plasmid-map"
                />
                <div className={css(styles.modalFooter)}>
                    <input
                        className={css(styles.enzymeInput)}
                        id="enzyme"
                        type="test"
                    />
                    <button className={css(styles.showEnzyme)} id="show_enzyme">
                        Show enzyme
                    </button>
                    <button className={css(styles.hideEnzyme)} id="hide_enzyme">
                        Hide enzyme
                    </button>
                </div>
            </div>
        </Modal>;
    },
});

const controlMargin = 5;

const styles = StyleSheet.create({
    enzymeInput: {
        marginLeft: controlMargin,
        marginRight: controlMargin,
    },
    modalContent: {
        width: "100%",
        height: "100%",
        display: "flex",
        alignItems: "center",
        justifyContent: "space-between",
        flexDirection: "column",
    },
    modalFooter: {
        width: "100%",
        display: "flex",
        alignItems: "center",
        justifyContent: "flex-start",
    },
    plasmidMap: {
        display: "flex",
        fontSize: ss.sizes.fontSizeCaption,
        justifyContent: "center",
        alignItems: "center",
        flexDirection: "column",
    },
    hideEnzyme: {
        marginLeft: controlMargin,
        marginRight: controlMargin,
    },
    showEnzyme: {
        marginLeft: controlMargin,
        marginRight: controlMargin,
    },
});

module.exports = PlasmidMap;
