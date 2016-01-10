const React = require("react");

const EditableBoolean = require("./editable-boolean.jsx");
const EditableText = require("./editable-text.jsx");
const {extVal} = require("./util.js");

const SequenceSection = React.createClass({
    propTypes: {
        data: React.PropTypes.any, // TODO
        editable: React.PropTypes.bool,
        makeUpdater: React.PropTypes.func,
        sequence: React.PropTypes.shape({
            sequence: React.PropTypes.shape({
                lookup: React.PropTypes.string,
            }),
            verified: React.PropTypes.shape({
                lookup: React.PropTypes.string,
            }),
        }),
        unsavedChanges: React.PropTypes.any, // TODO
    },
    val: function(l) {
        return extVal({...this.props.data, ...this.props.unsavedChanges}, l);
    },
    getVerifiedIcon: function() {
        const verified = this.val((this.props.sequence.verified || {}).lookup);
        if (verified) {
            return <i className="material-icons yes-icon">check</i>;
        } else if (verified === false) {
            return <i className="material-icons no-icon">close</i>;
        }
        return null;
    },

    render: function() {
        if (!this.props.sequence) {
            return <div></div>;
        }
        const verifiedLookup = (this.props.sequence.verified || {}).lookup;
        return <div className="sequence-section info-section">
            <div className="seq-label-row">
            <div className="seq-label-and-size">
            <div className="info-section-label seq" ref="seqLabel">
                Sequence
            </div>
            <div className="seq-size">
                &nbsp;({
                    (this.val(this.props.sequence.sequence.lookup) || []).length
                }bp)
            </div>
            </div>
            <div className="seq-verified">
                {verifiedLookup && this.props.editable ?
                    <div className="field-name">Verified</div> : null}
                {verifiedLookup ?
                    <EditableBoolean
                        editable={this.props.editable}
                        onChange={this.props.makeUpdater(verifiedLookup)}
                        value={this.val(verifiedLookup)}
                    /> :
                    null}
            </div>
            </div>
            <div className="info-section-contents" aria-labelledby="seqLabel">
                <div className="sequence field-value single major">
            <EditableText
                editable={this.props.editable}
                onChange={this.props.makeUpdater(
                    this.props.sequence.sequence.lookup)}
                single={true}
                value={this.val(this.props.sequence.sequence.lookup)}
            />
            </div>
            </div>
        </div>;

    },
});

module.exports = SequenceSection;
