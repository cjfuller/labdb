import React, { ReactNode } from "react";
import { StyleSheet, css } from "aphrodite";
import $ from "jquery";

import type { Scope } from "./auth";
import * as ae from "./action-executors";
import exportHandler from "./export-handler";
import ss from "./shared-styles";

type EntryProps = {
  children: ReactNode;
  href?: string | null | undefined;
  iconName: string;
  interactive?: boolean | undefined;
  onClick?: Function | undefined;
};

function HamburgerEntry(props: EntryProps) {
  const onClick = () => {
    if (props.href) {
      // TODO: make this not require a page reload.
      (window.location as any) = props.href;
    } else {
      props.onClick?.();
    }
  };
  return (
    <div
      className={css(
        styles.hamburgerEntry,
        props.interactive && styles.interactive
      )}
      onClick={onClick}
    >
      <div className={css(styles.icon)}>
        <i className="material-icons">{props.iconName}</i>
      </div>
      {props.children}
    </div>
  );
}

function HamburgerSectionName(props: { name: string }) {
  return (
    <div className={css(styles.hamburgerSectionName)}>
      <span>{props.name}</span>
    </div>
  );
}

function logout() {
  return $.ajax({
    url: "/logout",
    method: "POST",
  })
    .then(() => {
      return (window as any).gapi.auth2.getAuthInstance().signOut();
    })
    .then(() => {
      window.location.href = "/";
    });
}

type AuthProps = {
  auth: Scope;
  children: ReactNode;
  required: Scope;
};
/**
 * Control display of children based on auth scope.
 * (This is actually enforced server-side, and just so that we don't show
 * options that are forbidden anyway.)
 */
function Auth(props: AuthProps) {
  const { auth, required } = props;
  const meetsAuthRequirement =
    auth === "admin" ||
    (auth === "write" && required !== "admin") ||
    (auth === "read" && required === "read");
  if (meetsAuthRequirement) {
    return <div>{props.children}</div>;
  }
  return null;
}

type Props = {
  close: Function;
  context: "collection" | "item";
  getState: Function;
  user: {
    name: string;
    auth: Scope;
  };
};

function doUpload(elt: any) {
  return () => {
    const data = new FormData();
    console.log(elt.files);
    const files = [];
    for (let i = 0; i < elt.files.length; i++) {
      files.push(elt.files.item(i));
    }
    files
      .filter((f) => f.type === "application/json")
      .forEach((f, idx) => data.append(`file_${idx}`, f));
    $.ajax({
      url: "/api/v1/import",
      type: "POST",
      data: data,
      processData: false,
      contentType: false,
    }).then(() => window.location.reload());
  };
}

function promptUpload() {
  const input = document.createElement("input");
  input.type = "file";
  input.multiple = true;
  input.onchange = doUpload(input);
  input.click();
}

export default function Hamburger(props: Props) {
  return (
    <div className={css(styles.hamburgerMenu)}>
      <HamburgerSectionName name="Logged in as" />
      <HamburgerEntry iconName="person">
        <span className={css(styles.userName)}>{props.user.name}</span>
      </HamburgerEntry>
      <HamburgerEntry iconName="cloud_off" interactive={true} onClick={logout}>
        <span>Log out</span>
      </HamburgerEntry>
      <Auth required="admin" auth={props.user.auth}>
        <HamburgerSectionName name="Administration" />
        <HamburgerEntry href="/users" iconName="people" interactive={true}>
          Manage users
        </HamburgerEntry>
        <HamburgerEntry
          iconName="cloud_upload"
          interactive={true}
          onClick={promptUpload}
        >
          <span>Bulk import data</span>
        </HamburgerEntry>
        <HamburgerEntry iconName="cloud_download" interactive={false}>
          <span>Bulk export data</span>
          <span className={css(styles.comingSoon)}>Coming soon!</span>
        </HamburgerEntry>
      </Auth>
      {props.context === "item" ? (
        <div>
          <HamburgerSectionName name="Current item" />
          <Auth auth={props.user.auth} required="write">
            <HamburgerEntry
              iconName="content_copy"
              interactive={true}
              onClick={() => {
                props.close();
                ae.copyItem(props.getState());
              }}
            >
              <span>Make a duplicate</span>
            </HamburgerEntry>
            {props.context === "item" && props.getState().type === "plasmid" ? (
              <HamburgerEntry
                iconName="library_add"
                interactive={true}
                onClick={() => {
                  props.close();
                  ae.strainFromPlasmid(props.getState());
                }}
              >
                <span>Make a strain from this plasmid</span>
              </HamburgerEntry>
            ) : null}
          </Auth>
          <HamburgerEntry
            iconName="file_download"
            interactive={true}
            onClick={exportHandler(props.getState, "json")}
          >
            <span>Export as JSON</span>
          </HamburgerEntry>
          {props.getState().sequenceInfo ? (
            <HamburgerEntry
              iconName="file_download"
              interactive={true}
              onClick={exportHandler(props.getState, "fasta")}
            >
              <span>Export as FASTA</span>
            </HamburgerEntry>
          ) : null}
          <Auth auth={props.user.auth} required="write">
            <HamburgerEntry
              iconName="delete"
              interactive={true}
              onClick={() => ae.deleteItem(props.getState())}
            >
              <span>Delete</span>
            </HamburgerEntry>
          </Auth>
        </div>
      ) : null}
      <HamburgerSectionName name="All databases" />
      <HamburgerEntry
        href="/plasmids"
        iconName="folder_open"
        interactive={true}
      >
        <span>Plasmids</span>
      </HamburgerEntry>
      <HamburgerEntry href="/oligos" iconName="folder_open" interactive={true}>
        <span>Oligos</span>
      </HamburgerEntry>
      <HamburgerEntry
        href="/bacteria"
        iconName="folder_open"
        interactive={true}
      >
        <span>Bacteria</span>
      </HamburgerEntry>
      <HamburgerEntry href="/samples" iconName="folder_open" interactive={true}>
        <span>Samples</span>
      </HamburgerEntry>
      <HamburgerEntry
        href="/antibodies"
        iconName="folder_open"
        interactive={true}
      >
        <span>Antibodies</span>
      </HamburgerEntry>
      <HamburgerEntry href="/lines" iconName="folder_open" interactive={true}>
        <span>Worms</span>
      </HamburgerEntry>
      <HamburgerEntry
        href="/yeaststrains"
        iconName="folder_open"
        interactive={true}
      >
        <span>Yeast</span>
      </HamburgerEntry>
      <HamburgerEntry
        href="/rnai_clones"
        iconName="folder_open"
        interactive={true}
      >
        <span>RNAi clones</span>
      </HamburgerEntry>
    </div>
  );
}

const hamburgerBorderPx = 6;

const styles = StyleSheet.create({
  comingSoon: {
    color: ss.colors.labdbGreen,
    fontSize: 10,
    marginLeft: 30,
  },
  hamburgerEntry: {
    borderBottom: `1px solid ${ss.colors.borderColor}`,
    borderLeft: `${hamburgerBorderPx}px solid rgba(0,0,0,0)`,
    boxSizing: "border-box",
    display: "flex",
    flexDirection: "row",
    justifyContent: "flex-start",
    alignItems: "center",
    paddingLeft: ss.sizes.paddingPx,
    paddingRight: ss.sizes.paddingPx,
    width: "100%",
    height: "3.5em",
  },
  hamburgerSectionName: {
    display: "flex",
    alignItems: "flex-end",
    borderBottom: `1px solid ${ss.colors.borderColor}`,
    boxSizing: "border-box",
    paddingLeft: ss.sizes.paddingPx,
    paddingRight: ss.sizes.paddingPx,
    width: "100%",
    height: "3.5em",
    fontFamily: ss.fonts.contrast,
    fontWeight: ss.fonts.weights.emph as 600,
    verticalAlign: "baseline",
  },
  hamburgerMenu: {
    backgroundColor: "white",
    borderLeft: `4px solid ${ss.colors.labdbGreen}`,
    boxSizing: "border-box",
    fontFamily: ss.fonts.base,
    fontSize: ss.sizes.fontSizeMedium,
    position: "fixed",
    top: 0,
    height: "100vh",
    width: "300px",
    minWidth: "300px",
    overflowX: "hidden",
    overflowY: "auto",
    paddingTop: ss.sizes.navbarHeightPx,
    right: 0,
    zIndex: 10,
    ...ss.traits.shadowed,
  },
  icon: {
    display: "inline-block",
    paddingTop: 2,
    margin: `0 ${ss.sizes.paddingPx}px`,
  },
  interactive: {
    ":hover": {
      borderLeft: `${hamburgerBorderPx}px ` + `solid ${ss.colors.mutedBlue}`,
    },
    cursor: "pointer",
  },
  userName: {
    fontFamily: ss.fonts.contrast,
    fontWeight: ss.fonts.weights.emph as 600,
  },
});

module.exports = Hamburger;
