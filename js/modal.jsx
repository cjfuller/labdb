const React = require("react");
const {StyleSheet, css} = require("aphrodite");

const ss = require("./shared-styles.js");

const Modal = React.createClass({
    propTypes: {
        children: React.PropTypes.node,
        onClose: React.PropTypes.func,
    },
    dontClose: function(event) {
        event.preventDefault();
        event.stopPropagation();
        return false;
    },
    render: function() {
        return <div
            className={css(styles.modalBackdrop)}
            onClick={this.props.onClose}
        >
            <div
                className={css(styles.modalContainer)}
                onClick={this.dontClose}
            >
                {this.props.children}
            </div>
        </div>;
    },
});

const styles = StyleSheet.create({
    modalBackdrop: {
        position: "fixed",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        width: "100vw",
        height: "100vh",
        top: 0,
        left: 0,
        zIndex: 20,
        backgroundColor: "rgba(0, 0, 0, 0.6)",
    },
    modalContainer: {
        backgroundColor: "white",
        border: `1px solid ${ss.colors.borderColor}`,
        borderRadius: 5,
        fontFamily: ss.fonts.content,
        padding: 20,
        width: 600,
        height: 600,
    },
    plasmidMap: {
        width: "100%",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
    },
});

module.exports = Modal;
