import React, { useState } from "react";
import { StyleSheet, css } from "aphrodite";

import * as actions from "./actions";
import EditableField from "./editable-field";
import ss from "./shared-styles";

// TODO: determine dynamically; share code with nav
const Types = {
  Plasmid: "Plasmids",
  Oligo: "Oligos",
  Bacterium: "Bacteria",
  Sample: "Samples",
  Antibody: "Antibodies",
  Line: "TC",
  Yeaststrain: "Yeast",
  SeqLib: "SeqLib",
};

type TypeKey = keyof typeof Types;

type TypeSelectorProps = {
  onChange: Function;
  selected: string[];
};

function TypeSelector(props: TypeSelectorProps) {
  const typeSelectors = Object.keys(Types).map((k) => {
    return (
      <div
        key={k}
        onClick={() => props.onChange(k)}
        className={css(
          styles.typeSelector,
          props.selected.includes(k) && styles.typeSelectorSelected
        )}
      >
        {Types[k as TypeKey]}
      </div>
    );
  });
  return <div className={css(styles.typeSelectorOuter)}>{typeSelectors}</div>;
}

type Props = {
  dispatch: Function;
  doSearch: Function;
};

export default function SearchBar(props: Props) {
  const [searchTerms, setSearchTerms] = useState<string>("");
  const [includeSequence, setIncludeSequence] = useState<boolean>(false);
  const [person, setPerson] = useState<string | null>(null);
  const [types, setTypes] = useState<TypeKey[]>(
    Object.keys(Types) as TypeKey[]
  );

  const updateTypes = (type: TypeKey) => {
    if (types.includes(type)) {
      setTypes(types.filter((t) => t !== type));
    } else {
      setTypes(types.concat(type));
    }
  };

  const closeSearch = () => {
    props.dispatch(actions.searchVisibility(false));
  };

  const doSearch = () =>
    props.doSearch(searchTerms, includeSequence, person, types);

  return (
    <div className={css(styles.searchBarOuter)}>
      <div className={css(styles.searchBar)}>
        <div className={css(styles.searchBarRow)}>
          <div>Search:</div>
          <div className={css(styles.searchField)}>
            <EditableField
              autoFocus={true}
              editable={true}
              onChange={setSearchTerms}
              onEnter={doSearch}
              value={searchTerms}
            />
          </div>
          <div className={css(styles.searchButton)} onClick={doSearch}>
            Go
          </div>
        </div>
        <div className={css(styles.searchBarRow)}>
          <label className={css(styles.seqOption)}>
            <input
              type="checkbox"
              checked={includeSequence}
              value={includeSequence ? "true" : "false"}
              onChange={() => setIncludeSequence(!includeSequence)}
            />
            Include sequence in search?
          </label>
        </div>
        <div className={css(styles.searchBarRow)}>Filters:</div>
        <div className={css(styles.searchBarRow)}>
          <label>
            <div className={css(styles.inline, styles.smallLabel)}>Person:</div>
            <EditableField
              autoFocus={false}
              editable={true}
              extraStyles={[styles.inline, styles.searchField]}
              onChange={setPerson}
              value={person}
            />
          </label>
        </div>
        <div className={css(styles.searchBarRow)}>
          <div className={css(styles.inline, styles.smallLabel)}>
            Types to search:
            <TypeSelector onChange={updateTypes} selected={types} />
          </div>
        </div>
      </div>
      <div className={css(styles.searchBarArrow)}></div>
      <div className={css(styles.searchBarClose)} onClick={closeSearch}>
        <i className="material-icons">close</i>
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  inline: {
    display: "inline-block",
  },
  searchBar: {
    alignItems: "flex-start",
    display: "flex",
    flexDirection: "column",
    justifyContent: "flex-start",
    width: "100%",
  },
  searchBarRow: {
    alignItems: "center",
    display: "flex",
    flexDirection: "row",
    justifyContent: "flex-start",
    marginBottom: 10,
    marginTop: 10,
    whiteSpace: "nowrap",
    width: "100%",
  },
  searchBarOuter: {
    backgroundColor: "white",
    border: `1px solid ${ss.colors.borderColor}`,
    borderBottom: `1px solid ${ss.colors.borderColor}`,
    borderLeft: `1px solid ${ss.colors.borderColor}`,
    borderRadius: 4,
    display: "flex",
    fontFamily: ss.fonts.base,
    justifyContent: "space-between",
    minWidth: 475,
    paddingLeft: 10,
    position: "fixed",
    right: 0,
    top: ss.sizes.navbarHeightPx,
    width: "30vw",
    zIndex: 21,
    ...ss.traits.shadowed,
  },
  searchBarArrow: {
    position: "absolute",
    top: -15,
    right: 1.5 * ss.sizes.hamburgerWidthPx - 8,
    border: "8px solid rgba(0, 0, 0, 0)",
    borderBottom: "8px solid white",
    zIndex: 21,
  },
  searchBarClose: {
    ":hover": {
      cursor: "pointer",
    },
  },
  searchButton: {
    display: "flex",
    flexShrink: 0,
    border: `1px solid ${ss.colors.borderColor}`,
    alignItems: "center",
    justifyContent: "center",
    borderRadius: 3,
    height: "1.2em",
    marginLeft: 10,
    width: 50,
    ":hover": {
      cursor: "pointer",
    },
    ":active": {
      backgroundColor: ss.colors.bitDarkBackground,
    },
  },
  searchField: {
    flexShrink: 1,
    height: "1.2em",
    marginLeft: 10,
    minWidth: 175,
    width: "80%",
  },
  seqOption: {
    flexShrink: 0,
    fontSize: ss.sizes.fontSizeCaption,
    maxHeight: ss.sizes.navbarHeightPx,
  },
  smallLabel: {
    fontSize: ss.sizes.fontSizeNormal,
  },
  typeSelectorOuter: {
    display: "inline-flex",
    alignItems: "center",
    justifyContent: "flex-start",
    marginLeft: 10,
  },
  typeSelector: {
    boxSizing: "border-box",
    border: `1px solid ${ss.colors.borderColor}`,
    fontSize: ss.sizes.fontSizeCaption,
    padding: 5,
    ":not(:last-of-type)": {
      borderRight: "none",
    },
    ":hover": {
      cursor: "pointer",
      borderBottom: `1px solid ${ss.colors.mutedBlueMoreOpaque}`,
    },
  },
  typeSelectorSelected: {
    backgroundColor: ss.colors.mutedBlueSemitransparent,
  },
});
