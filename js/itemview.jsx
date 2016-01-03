const React = require("react");
const classNames = require("classnames");
const _ = require("underscore");

const ActionExecutors = require("action-executors");

const RPT = React.PropTypes;

function extVal(data, lookup) {
    return lookup && data[lookup];
}

const DecrementButton = React.createClass({
    propTypes: {
        clickCallback: RPT.func,
    },
    render: function() {
        return <div
            className="decrement-button"
            onClick={this.props.clickCallback}
        >
            Remove one stock
        </div>;
    },
});

const InventoryWidget = React.createClass({
    propTypes: {
        inventory: RPT.shape({
            count: RPT.number,
            clone: RPT.string,
            date: RPT.string,
            location: RPT.string,
            person: RPT.string,
        }),
    },
    inventoryDecrementAction: function(invitem) {
        return function() {
            console.log("hello, world")
            $.ajax({
                // TODO: don't rely on window location here.
                url: (
                    window.location.pathname +
                    "/update_number?inc=-1&location=" +
                    JSON.stringify(invitem)),
                method: "PUT",
                // TODO: this redirects to the page we were already on, but we
                // really should ditch the redirect.
                success: function(data, status) {
                    window.location.reload();
                },
                error: function(xhr, status, errorthrown) {
                    window.location.reload();
                },
                async: false,
            });
        };
    },

    render: function() {
        if (!this.props.inventory) {
            return <div></div>;
        }
        return <table className="inventory-table">
        <thead>
            <tr>
                <th>Location</th>
                <th>Stock count</th>
                <th>Clone</th>
                <th>Person</th>
                <th>Date</th>
                <th></th>
            </tr>
        </thead>
        <tbody>
        {_.map(this.props.inventory, (invitem) => {
            return <tr>
                <td>{invitem.location}</td>
                <td>{invitem.count}</td>
                <td>{invitem.clone}</td>
                <td>{invitem.person}</td>
                <td>{invitem.date}</td>
                <td>
                    <DecrementButton
                        clickCallback={this.inventoryDecrementAction(invitem)}
                    />
                </td>
            </tr>;
        })}
        </tbody>
        </table>;
    },
});

const MegaBar = React.createClass({
    propTypes: {
        data: RPT.shape({
            coreLinks: RPT.any,  // TODO
            fieldData: RPT.any,  // TODO
            name: RPT.string,
            shortDesc: RPT.any, // TODO
        }),
        editable: RPT.bool,
        unsavedChanges: RPT.any, // TODO
        updater: RPT.func,
    },
    getCoreLinks: function() {
        if (this.props.data.coreLinks) {
            return <div className="field-value">
                {this.props.editable ?
                 <EditableText
                     editable={this.props.editable}
                     onChange={this.props.updater(
                         this.props.data.coreLinks.lookup)}
                     value={extVal(
                         this.getFieldData(),
                         this.props.data.coreLinks.lookup)}
                 /> :
                 this.props.data.coreLinks.inlineValue.map((link, i) => {
                     return <div
                         className="core-links"
                         key={i}
                         dangerouslySetInnerHTML={{
                             __html: this.props.data.coreLinks.inlineValue[i],
                         }}
                     />;
                 })}
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
                        onChange={this.props.updater(
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

const SequenceSection = React.createClass({
    propTypes: {
        data: RPT.any, // TODO
        editable: RPT.bool,
        sequence: RPT.shape({
            sequence: RPT.shape({
                lookup: RPT.string,
            }),
            verified: RPT.shape({
                lookup: RPT.string,
            }),
        }),
        unsavedChanges: RPT.any, // TODO
        updater: RPT.func,
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
                        onChange={this.props.updater(verifiedLookup)}
                        value={this.val(verifiedLookup)}
                    /> :
                    null}
            </div>
            </div>
            <div className="info-section-contents" aria-labelledby="seqLabel">
                <div className="sequence field-value single major">
            <EditableText
                editable={this.props.editable}
                onChange={this.props.updater(
                    this.props.sequence.sequence.lookup)}
                single={true}
                value={this.val(this.props.sequence.sequence.lookup)}
            />
            </div>
            </div>
        </div>;

    },
});

const InfoSection = React.createClass({
    propTypes: {
        contents: RPT.any, // TODO
        data: RPT.any, // TODO
        editable: RPT.bool,
        name: RPT.string,
        unsavedChanges: RPT.any, // TODO
        updater: RPT.func,
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
                        onChange={this.props.updater(
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
        return _.map(this.props.contents.fields, (f) => {
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
                         onChange={this.props.updater(f.lookup)}
                         unknownMarker="?"
                         value={this.val(f.lookup)}
                     /> :
                     <EditableText
                         editable={this.props.editable}
                         onChange={this.props.updater(f.lookup)}
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

const CoreInfo = React.createClass({
    propTypes: {
        data: RPT.any, // TODO
        editable: RPT.bool,
        unsavedChanges: RPT.any, // TODO
        updater: RPT.func,
    },
    render: function() {
        return <div className="core-info nine columns">
            {_.map(this.props.data.coreInfoSections, (s) => {
                return <InfoSection
                    data={this.props.data.fieldData}
                    updater={this.props.updater}
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
                updater={this.props.updater}
            />
            <InventoryWidget inventory={this.props.data.inventory} />
        </div>;
    },
});

const SupplementalField = React.createClass({
    propTypes: {
        data: RPT.any, // TODO
        editable: RPT.bool,
        item: RPT.any, // TODO
        onChange: RPT.func,
    },
    render: function() {
        const f = this.props.item;
        const labelRef = "label-" + f.name;
        return <div className="supplemental-item">
            <div className="field-name suppl" ref={labelRef} key={labelRef}>
                {f.name + ":"}
            </div>
            <div
                aria-labelledby={this.refs.labelRef}
                className="field-value suppl"
                ref={"value-" + f.name}
            >
                <EditableText
                    value={extVal(this.props.data, f.lookup)}
                    editable={this.props.editable}
                    onChange={this.props.onChange}
                />
            </div>
        </div>;
    },
});

const BooleanEditingControl = React.createClass({
    propTypes: {
        onChange: RPT.func,
        value: RPT.bool,
    },
    setValue: function(val) {
        return (e) => this.props.onChange(val);
    },

    render: function() {
        return <div className="boolean-editor">
            <div
                className={classNames({"boolean-editor-yes": true,
                                       selected: this.props.value === true})}
                onClick={this.setValue(true)}
            >
                {"\u2714"}
            </div>
            <div
                className={classNames({"boolean-editor-no": true,
                                       selected: this.props.value === false})}
                onClick={this.setValue(false)}
            >
                {"\u2716"}
            </div>
            <div
                className={classNames({
                    "boolean-editor-unknown": true,
                    selected: (this.props.value === null ||
                               this.props.value === undefined),
                })}
                onClick={this.setValue(null)}
            >
                ?
            </div>
        </div>;
    },
});

const EditableBoolean = React.createClass({
    propTypes: {
        editable: RPT.bool,
        unknownMarker: RPT.string,
        value: RPT.bool,
    },
    getDefaultProps: function() {
        return {unknownMarker: ""};
    },
    getIconValue: function(v) {
        if (v === true) {
            return <div className="yes-icon">
                <i className="material-icons">check</i>
            </div>;
        } else if (v === false) {
            return <div className="no-icon">
                <i className="material-icons">close</i>
            </div>;
        } else {
            return <div className="unknown-icon">
                {this.props.unknownMarker}
            </div>;
        }
    },

    render: function() {
        return this.props.editable ?
            <BooleanEditingControl {...this.props} /> :
            this.getIconValue(this.props.value);
    },
});


const EditableField = React.createClass({
    propTypes: {
        editable: RPT.bool,
        fieldClasses: RPT.any, // TODO
        onChange: RPT.func,
        value: RPT.node,
    },
    render: function() {
        return <input
            className={classNames({
                editableField: true,
                editable: this.props.editable,
            }, this.props.fieldClasses)}
            disabled={!this.props.editable}
            onChange={(e) => this.props.onChange(e.target.value)}
            type="text"
            value={this.props.value}
        />;
    },
});

const EditableArea = React.createClass({
    propTypes: {
        editable: RPT.bool,
        fieldClasses: RPT.any, // TODO
        onChange: RPT.func,
        value: RPT.string,
    },
    render: function() {
        return this.props.editable ?
            <textarea
                className={classNames({
                    editableField: true,
                    editable: this.props.editable,
                }, this.props.fieldClasses)}
                disabled={!this.props.editable}
                onChange={(e) => this.props.onChange(e.target.value)}
                value={this.props.value}
            /> :
            <div>{this.props.value}</div>;
    }});

const EditableText = React.createClass({
    propTypes: {
        single: RPT.bool,
    },
    render: function() {
        return (this.props.single ?
                <EditableArea {...this.props} /> :
                <EditableField {...this.props} />);
    }});


const SupplementalInfo = React.createClass({
    propTypes: {
        data: RPT.any, // TODO
        editable: RPT.bool,
        unsavedChanges: RPT.any, // TODO
        updater: RPT.func,
    },
    getInitialState: function() {
        return {
            plasmidModalOpen: false,
        };
    },

    getFieldData: function() {
        if (this.props.editable) {
            return {...this.props.data.fieldData, ...this.props.unsavedChanges};
        }
        return this.props.data.fieldData;
    },

    render: function() {
        return <div className="three columns">
            <div className="supplemental-info">
            {_.map(this.props.data.supplementalFields, (f) => {
                return <SupplementalField
                    data={this.getFieldData()}
                    editable={this.props.editable}
                    item={f}
                    key={f.name}
                    onChange={this.props.updater(f.lookup)}
                />;
            })}
            </div>
            {(this.props.data.type === "plasmid") ?
                <input
                    className="plasmap-button"
                    type="button"
                    value="Plasmid map"
                /> :
                null}
        </div>;
    },
});


const ItemInfoView = React.createClass({
    propTypes: {
        data: RPT.any, // TODO
        editable: RPT.bool,
        unsavedChanges: RPT.any, // TODO
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

    updater: function(key) {
        return (value) => this.onUpdate(key, value);
    },

    render: function() {
        return <div className="item-info-view">
            <MegaBar
                data={this.props.data}
                editable={this.props.editable}
                unsavedChanges={this.props.unsavedChanges}
                updater={this.updater}
            />
            <div className="row info-row">
                <CoreInfo
                    data={this.props.data}
                    editable={this.props.editable}
                    unsavedChanges={this.props.unsavedChanges}
                    updater={this.updater}
                />
                <SupplementalInfo
                    data={this.props.data}
                    editable={this.props.editable}
                    unsavedChanges={this.props.unsavedChanges}
                    updater={this.updater}
                />
        </div>
        </div>;
    },
});

module.exports = ItemInfoView;
