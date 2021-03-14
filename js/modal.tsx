import React, { ReactNode, MouseEvent } from "react";
import { StyleSheet, css } from "aphrodite";

import ss from "./shared-styles";

const dontClose = (event: MouseEvent) => {
  event.preventDefault();
  event.stopPropagation();
  return false;
};

type Props = {
  children: ReactNode;
  onClose: (e: MouseEvent) => void;
};

export default function Modal(props: Props) {
  return (
    <div className={css(styles.modalBackdrop)} onClick={props.onClose}>
      <div className={css(styles.modalContainer)} onClick={dontClose}>
        {props.children}
      </div>
    </div>
  );
}

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
