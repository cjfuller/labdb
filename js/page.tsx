import React, { MouseEvent } from "react";
import _ from "underscore";
import { StyleSheet, css } from "aphrodite";

import * as Actions from "./actions";
import * as ActionExecutors from "./action-executors";
import Hamburger from "./hamburger";
import ItemTable from "./itemlist";
import ItemInfoView from "./itemview";
import Navbar from "./nav";
import PlasmidMap from "./plasmid-map";
import ss from "./shared-styles";
import SearchBar from "./search";

type Props = {
  data: any;
  dispatch: Function;
  editMode: boolean;
  mapVisible: boolean;
  showHamburger: boolean;
  showSearch: boolean;
  unsavedChanges: any;
  user: {
    name: string;
    email: string;
    auth: "admin" | "write" | "read";
  };
};

export default function Page(props: Props) {
  const editToggle = () => {
    if (props.editMode && Object.keys(props.unsavedChanges).length > 0) {
      ActionExecutors.saveEdits(
        _.pick(props.data, ["type", "id", "resourcePath"]),
        props.unsavedChanges
      );
    } else if (!props.editMode) {
      props.dispatch(Actions.setEditMode(true));
    } else {
      props.dispatch(Actions.setEditMode(false));
    }
  };
  const cancelEdits = () => {
    ActionExecutors.clearEdits(
      _.pick(props.data, ["type", "id", "resourcePath"])
    );
  };

  const toggleBurger = (_: MouseEvent) => {
    props.dispatch(Actions.hamburgerVisibility(!props.showHamburger));
  };

  const doSearch = (
    searchTerm: string,
    includeSequence: boolean,
    person: string,
    types: string[]
  ) => {
    Actions.doSearchAndRedirect(searchTerm, includeSequence, person, types);
  };

  const closePlasmidMap = () => {
    props.dispatch(Actions.mapVisibility(false));
  };

  let hamburgerContext: "item" | "collection" | null = null;
  let pageContent = null;
  switch (props.data.type) {
    case "search":
      hamburgerContext = "collection";
      if (!props.data.items) {
        pageContent = (
          <div className={css(styles.noResults)}>No results found.</div>
        );
      } else {
        pageContent = (
          <ItemTable data={props.data} sort={["timestamp", "id"]} />
        );
      }
      break;
    case "collection":
      hamburgerContext = "collection";
      pageContent = <ItemTable data={props.data} />;
      break;
    default:
      hamburgerContext = "item";
      pageContent = (
        <ItemInfoView
          data={props.data}
          editable={props.editMode}
          unsavedChanges={props.unsavedChanges}
        />
      );
  }
  const plasmidMapModal = props.mapVisible ? (
    <PlasmidMap
      data={props.data.plasmid_map}
      onClose={closePlasmidMap}
      plasmid={props.data}
    />
  ) : null;
  return (
    <div id="page">
      <Navbar
        cancelEditCallback={cancelEdits}
        data={props.data}
        dispatch={props.dispatch}
        editCallback={editToggle}
        editMode={props.editMode}
        onClickHamburger={toggleBurger}
      />
      {props.showSearch ? (
        <SearchBar dispatch={props.dispatch} doSearch={doSearch} />
      ) : null}
      {props.showHamburger ? (
        <Hamburger
          close={toggleBurger}
          context={hamburgerContext}
          getState={() => props.data}
          user={props.user}
        />
      ) : null}
      {plasmidMapModal}
      <div
        className={css(styles.pageContainerOuter)}
        onClick={props.showHamburger ? toggleBurger : () => null}
      >
        <div className={css(styles.pageContainer)}>{pageContent}</div>
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  noResults: {
    marginTop: 10,
  },
  pageContainer: {
    margin: "0 auto",
    maxWidth: "80vw",
    padding: 3 * ss.sizes.paddingPx,
    paddingTop: ss.sizes.navbarHeightPx,
  },
  pageContainerOuter: {
    boxSizing: "border-box",
    fontFamily: ss.fonts.base,
    fontSize: ss.sizes.fontSizeNormal,
    height: "100vh",
    overflowX: "hidden",
    overflowY: "auto",
    maxWidth: "100vw",
  },
});
