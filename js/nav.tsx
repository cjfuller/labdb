import React, { MouseEvent } from "react";
import { css, StyleSheet } from "aphrodite";

import * as actions from "./actions";
import * as ae from "./action-executors";
import auth from "./auth";
import ss from "./shared-styles";

type NavItemProps = {
  addr: string;
  name: string;
};

function NavItem(props: NavItemProps) {
  const onClick = () => {
    // TODO: add to API and do this instead
    // window._store.updateDataFromURL(
    //     `${this.dynamicResourceBase}${this.props.addr}`);
    window.location.href = props.addr;
  };

  return (
    <div className={css(styles.navitemContainer)} onClick={onClick}>
      <div className={css(styles.navitem)}>{props.name}</div>
    </div>
  );
}

type NavLogoProps = {
  text: string;
};

function NavLogo(props: NavLogoProps) {
  const goHome = () => {
    window.location.pathname = "/";
  };
  return (
    <div className={css(styles.navlogo)} id="labdb" onClick={goHome}>
      {props.text}
    </div>
  );
}

type CtxActionProps = {
  cancelEditCallback: (e: MouseEvent) => void;
  data: any;
  editCallback: (e: MouseEvent) => void;
  editMode: boolean;
};

function CtxActions(props: CtxActionProps) {
  const isCollection =
    props.data.type === "collection" || props.data.type == "search";
  const doNextCollection = () => {
    // TODO: handle ascending sort
    const desc = true;
    let nextIndex = null;
    let sortOrder = null;
    // TODO: what if there's no items?  Should probably disable buttons.
    if (desc) {
      nextIndex =
        props.data.items[props.data.items.length - 1].fieldData[
          props.data.numberFieldName
        ] - 1;
      sortOrder = "DESC";
    } else {
      nextIndex =
        props.data.items[props.data.items.length - 1].fieldData[
          props.data.numberFieldName
        ] + 1;
      sortOrder = "ASC";
    }
    // TODO: make this not require a reload using:
    /* ae.maybeFetchThenDisplay("table", {
           type: this.props.data.items[0].type,
           start: nextIndex,
           }, sortOrder); */
    window.location.search = `sort_order=${sortOrder}&start=${nextIndex}`;
  };
  const doNext = () => {
    // TODO: make this not require a reload.
    window.location.href = `${props.data.resourcePath}/next`;
  };
  const doPrevious = () => {
    // TODO: make this not require a reload.
    window.location.href = `${props.data.resourcePath}/previous`;
  };
  const doPreviousCollection = () => {
    // TODO: handle ascending sort
    const desc = true;
    let nextIndex = null;
    let sortOrder = null;
    // TODO: is there a way to make this symmetric with the next
    // button in the face of missing items?
    const offset = 100;
    // TODO: what if there's no items?  Should probably disable buttons.
    if (desc) {
      nextIndex =
        props.data.items[0].fieldData[props.data.numberFieldName] + offset;
      sortOrder = "DESC";
    } else {
      nextIndex =
        props.data.items[0].fieldData[props.data.numberFieldName] - offset;
      sortOrder = "ASC";
    }
    window.location.search = `sort_order=${sortOrder}&start=${nextIndex}`;
  };
  return (
    <div className={css(styles.ctxactions)}>
      <div
        className={css(styles.action)}
        onClick={isCollection ? doPreviousCollection : doPrevious}
        title="previous"
      >
        <i className="material-icons">arrow_back</i>
      </div>
      <div
        className={css(styles.action)}
        onClick={isCollection ? doNextCollection : doNext}
        title="next"
      >
        <i className="material-icons">arrow_forward</i>
      </div>
      {props.editMode && auth("write") && !isCollection ? (
        <div
          className={css(styles.action)}
          onClick={props.cancelEditCallback}
          title="discard changes"
        >
          <i className="material-icons">block</i>
        </div>
      ) : null}
      {isCollection || !auth("write") ? null : (
        <div
          className={css(styles.action)}
          onClick={props.editCallback}
          title={props.editMode ? "save changes" : "edit"}
        >
          {props.editMode ? (
            <i className="material-icons">save</i>
          ) : (
            <i className="material-icons">mode_edit</i>
          )}
        </div>
      )}
    </div>
  );
}

type ActionProps = {
  // TODO: prop for whether to display new
  // TODO: correct path for search
  cancelEditCallback: (e: MouseEvent) => void;
  data: any;
  dispatch: Function;
  editCallback: (e: MouseEvent) => void;
  editMode: boolean;
  onClickHamburger: (e: MouseEvent) => void;
};

function Actions(props: ActionProps) {
  const newItem = () => {
    const { data } = props;
    const type = data.type === "collection" ? data.itemType : data.type;
    if (type) {
      ae.newItem(type);
    }
  };

  const doSearch = () => {
    props.dispatch(actions.searchVisibility(true));
  };

  return (
    <div className={css(styles.actions)}>
      <CtxActions
        cancelEditCallback={props.cancelEditCallback}
        data={props.data}
        editCallback={props.editCallback}
        editMode={props.editMode}
      />
      <div className={css(styles.fixactions)}>
        {auth("write") && props.data["type"] !== "search" ? (
          <div className={css(styles.action)} onClick={newItem}>
            <i className="material-icons">add</i>
          </div>
        ) : null}
        <div className={css(styles.action)} onClick={doSearch}>
          <i className="material-icons">search</i>
        </div>
        <div className={css(styles.hamburgerWrapper)}>
          <div className={css(styles.action)} onClick={props.onClickHamburger}>
            <i className="material-icons">menu</i>
          </div>
        </div>
      </div>
    </div>
  );
}

type Props = ActionProps & {
  navitems?: string[] | undefined;
  navlinks?: any | undefined;
};

export default function Navbar(props: Props) {
  const navitems = props.navitems ?? [
    "Bacteria",
    "Worms",
    "Plasmids",
    "Oligos",
    "Antibodies",
    "Samples",
    "RNAiClones",
  ];
  const navlinks = props.navlinks ?? {
    Plasmids: "/plasmids",
    Oligos: "/oligos",
    Bacteria: "/bacteria",
    Samples: "/samples",
    Antibodies: "/antibodies",
    Worms: "/lines",
    RNAiClones: "/rnai_clones",
  };
  return (
    <div className={css(styles.navbar)}>
      <div className={css(styles.navbarTextSection)}>
        <NavLogo text={"LabDB2.\u03b2"} />
        {navitems.map((n, i) => {
          return <NavItem addr={navlinks[n]} name={n} key={n} />;
        })}
      </div>
      <Actions
        cancelEditCallback={props.cancelEditCallback}
        data={props.data}
        dispatch={props.dispatch}
        editCallback={props.editCallback}
        editMode={props.editMode}
        onClickHamburger={props.onClickHamburger}
      />
    </div>
  );
}

const styles = StyleSheet.create({
  action: {
    alignItems: "center",
    display: "flex",
    ":hover": {
      cursor: "pointer",
    },
    justifyContent: "center",
    width: ss.sizes.hamburgerWidthPx,
  },
  actions: {
    alignContent: "center",
    display: "flex",
    flexDirection: "row",
    flexShrink: 0,
    height: ss.sizes.navbarHeightPx,
    justifyContent: "flex-end",
  },
  ctxactions: {
    alignItems: "center",
    display: "flex",
    flexDirection: "row",
    justifyContent: "flex-end",
  },
  fixactions: {
    alignItems: "center",
    display: "flex",
    flexDirection: "row",
  },
  hamburgerWrapper: {
    borderLeft: "1px solid black",
    boxSizing: "border-box",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    height: ss.sizes.navbarHeightPx,
    width: ss.sizes.hamburgerWidthPx,
  },
  navbar: {
    backgroundColor: ss.colors.labdbGreen,
    boxShadow: "0px 0px 4px 2px rgba(0, 0, 0, 0.2)",
    boxSizing: "border-box",
    display: "flex",
    fontFamily: ss.fonts.base,
    fontSize: ss.sizes.fontSizeLarge,
    height: ss.sizes.navbarHeightPx,
    justifyContent: "space-between",
    left: 0,
    position: "fixed",
    top: 0,
    width: "100vw",
    zIndex: 20,
  },
  navbarTextSection: {
    alignItems: "center",
    display: "flex",
    flexShrink: 1,
    marginLeft: ss.sizes.paddingPx,
    overflowX: "hidden",
  },
  navitem: {
    display: "inline-block",
    marginLeft: ss.sizes.paddingPx,
    marginRight: ss.sizes.paddingPx,
  },
  navitemContainer: {
    borderBottom: "3px solid rgba(0,0,0,0)",
    boxSizing: "border-box",
    display: "flex",
    flexDirection: "row",
    justifyContent: "center",
    alignItems: "center",
    ":hover": {
      cursor: "pointer",
      borderBottom: `3px solid ${ss.colors.mutedBlue}`,
    },
    height: ss.sizes.navbarHeightPx,
  },
  navlogo: {
    boxSizing: "border-box",
    fontFamily: "Rokkitt, serif",
    fontSize: "150%",
    paddingLeft: 2 * ss.sizes.paddingPx,
    paddingRight: 4 * ss.sizes.paddingPx,
    ":hover": {
      cursor: "pointer",
    },
  },
});
