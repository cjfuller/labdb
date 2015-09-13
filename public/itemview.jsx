var DecrementButton = React.createClass({
    render: function() {
        return <div className="decrement-button"
                    onClick={this.props.clickCallback}>Remove one stock</div>;
    },
});

var InventoryWidget = React.createClass({
    inventoryDecrementAction: function(invitem) {
        return function() {
            $.ajax({
                // TODO: don't rely on window location here.
                url: window.location.pathname + "/update_number?inc=-1&location=" + JSON.stringify(invitem),
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
    render: function () {
        if (!this.props.inventory) {
            return <div></div>;
        }
        return <table className="inventory-table">
        <thead>
        <tr><th>Location</th>
            <th>Stock count</th>
            <th>Clone</th>
            <th>Person</th>
            <th>Date</th>
            <th></th></tr>
        </thead>
        <tbody>
        {_.map(this.props.inventory, function(invitem) {
            return <tr>
                <td>{invitem.location}</td>
                <td>{invitem.count}</td>
                <td>{invitem.clone}</td>
                <td>{invitem.person}</td>
                <td>{invitem.date}</td>
                <td><DecrementButton clickCallback={this.inventoryDecrementAction(invitem)}/></td>
            </tr>;
        }.bind(this))}
        </tbody>
        </table>;
    },
});

var MegaBar = React.createClass({
    getCoreLinks: function() {
        if (this.props.data.coreLinks) {
            return <div className="core-links">
                {_.map(this.props.data.coreLinks, function(clh) {
                    return (
                        <div className="core-link">
                        <div className="field-value"
                             dangerouslySetInnerHTML={{__html: clh}} /></div>);
                })}
            </div>;
        }
        return null;
    },

    render: function() {
        return <div className="item-megabar">
        <div className="item-id">
        {this.props.data.name}
        </div>
        <div className="row">
        <div className="nine columns">
        <div className="alias-field">
        <div className="field-value">
            <div className="item-alias" dangerouslySetInnerHTML={{__html: this.props.data.shortDesc}} />
        </div>
        </div>
        <div className="linked-items">
            {this.getCoreLinks()}
        </div>
        </div>
        <div className="three columns">
        </div>
        </div>

        </div>;
    },
});

var SequenceSection = React.createClass({
    getVerifiedIcon: function() {
        if (this.props.sequence.verified === true) {
            return <i className="material-icons yes-icon">check</i>;
        } else if (this.props.sequence.verified === false) {
            return <i className="material-icons no-icon">close</i>;
        }
        return null;
    },

    render: function() {
        if (!this.props.sequence) {
            return <div></div>;
        }
        return <div className="sequence-section">
            <div className="seq-label-row">
            <div className="info-section-label seq" ref="seqLabel">
                Sequence
            </div>
            <div className="seq-size">
                &nbsp;({(this.props.sequence.sequence || []).length}bp)
            </div>
            <div className="seq-verified">
                {this.getVerifiedIcon()}
            </div>
            </div>
            <div className="info-section-contents" aria-labelledby="seqLabel">
                <div className="sequence">
                {this.props.sequence.sequence}
                </div>
            </div>
        </div>;

    },
});

var InfoSection = React.createClass({
    getIconValue: function(v) {
        if (v === true) {
            return <div className="yes-icon">{"\u2714"}</div>;
        } else if (v === false) {
            return <div className="no-icon">{"\u2716"}</div>;
        } else {
            return <div>?</div>;
        }
    },
    getSectionContents: function() {
        if (this.props.contents.inlineValue) {
            if (this.props.contents.preformatted) {
                return <div
                    className="field-value"
                    dangerouslySetInnerHTML={{__html: this.props.contents.inlineValue}} />;
            }

            return <div className="field-value">
                {this.props.contents.inlineValue}
            </div>;
        }
        return _.map(this.props.contents.fields, function(f) {
            var isBooleanField = f.type && f.type === "boolean";
            var sep = isBooleanField ? "?" : ":";
            var valueTransformer = isBooleanField ? this.getIconValue : function(x) {return x;};

            var labelRef = "label-" + f.name;
            return <div key={f.name}>
            <div className="field-name" ref={labelRef} key={labelRef}>
                {f.name + sep}
            </div>
            <div className="field-value" aria-labelledby={this.refs[labelRef]} key={"value-" + f.name}>
                {valueTransformer(f.value)}
            </div>
            </div>;
        }.bind(this));
    },

    render: function() {
        return <div className="info-section">
            <div className="info-section-label" ref="sectionLabel">
                {this.props.name}
            </div>
            <div className="info-section-contents" aria-labelledby={this.refs.sectionLabel}>
                {this.getSectionContents()}
            </div>
        </div>;
    },

});

var CoreInfo = React.createClass({

    render: function() {
        return <div className="core-info nine columns">
            {_.map(this.props.data.coreInfoSections, function(s) {
                return <InfoSection key={s.name} name={s.name} contents={s} />;
            })}
            <SequenceSection sequence={this.props.data.sequenceInfo}/>
            <InventoryWidget inventory={this.props.data.inventory}/>
        </div>;
    },
});

var SupplementalInfo = React.createClass({

    getInitialState: function() {
        return {
            plasmidModalOpen: false,
        };
    },

    render: function() {
        return <div className="three columns">
            <div className="supplemental-info">
            {_.map(this.props.data.supplementalFields, function(f) {
                var labelRef = "label-" + f.name;
                return <div key={f.name} className="supplemental-item">
                    <div className="field-name suppl" ref={labelRef} key={labelRef}>
                        {f.name + ":"}
                    </div>
                    <div className="field-value suppl" ref={"value-" + f.name}
                         aria-labelledby={this.refs.labelRef}>
                        {f.value}
                    </div>
                </div>;
            }.bind(this))}
            </div>
            {(this.props.data.type === "plasmid") ?
                <input value="Plasmid map" type="button" className="plasmap-button"/> :
                null}
        </div>;
    },
});


window.ItemInfoView = React.createClass({
    render: function() {
        return <div className="item-info-view">
        <MegaBar data={this.props.data} />
        <div className="row info-row">
        <CoreInfo data={this.props.data} />
        <SupplementalInfo data={this.props.data} />
        </div>
        </div>;
    },
});
