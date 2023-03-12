import React, { useEffect } from "react";
import { StyleSheet, css } from "aphrodite";
import $ from "jquery";

import ss from "./shared-styles";

const clientID =
  "146923434465-alq7iagpanjvoag20smuirj0ivdtfldk.apps.googleusercontent.com";

function onSignIn({ credential }: { credential: string }) {
  return $.ajax({
    url: `/api/verify?jwt=${credential}`,
    method: "POST",
  }).then(() => {
    // TODO: make a logged in landing page.
    window.location.href = "/plasmids";
  });
}

export default function LandingPage() {
  useEffect(() => {
    (window as any).google.accounts.id.initialize({
      auto_select: true,
      client_id: clientID,
      callback: onSignIn,
    });
    (window as any).google.accounts.id.renderButton(
      document.getElementById("google-signin-button"),
      { theme: "outline", size: "large" }
    );

    (window as any).google.accounts.id.prompt();
  }, []);
  return (
    <div className={css(styles.container)}>
      <div className={css(styles.box)}>
        <div className={css(styles.title)}>
          Welcome to the {(window as any)._labdbName}!
        </div>
        <div>Please log in.</div>
        <div className={css(styles.prompt)}>
          <div id="google-signin-button"></div>
        </div>
      </div>
    </div>
  );
}

const styles = StyleSheet.create({
  box: {
    alignItems: "center",
    border: `2px solid ${ss.colors.labdbGreen}`,
    borderRadius: 5,
    display: "flex",
    flexBasis: 540,
    flexDirection: "column",
    fontFamily: ss.fonts.base,
    fontSize: ss.sizes.fontSizeExtraLarge,
    justifyContent: "space-around",
    margin: ss.sizes.paddingPx,
    minHeight: 350,
  },
  container: {
    alignItems: "center",
    display: "flex",
    flexDirection: "row",
    height: "100vh",
    justifyContent: "center",
    width: "100vw",
  },
  prompt: {
    alignItems: "center",
    display: "flex",
    flexDirection: "column",
    justifyContent: "space-around",
    width: "100%",
  },
  title: {
    fontFamily: ss.fonts.contrast,
    fontSize: 30,
  },
});
