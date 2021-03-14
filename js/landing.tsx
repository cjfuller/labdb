import React, { useEffect } from "react";
import { StyleSheet, css } from "aphrodite";
import $ from "jquery";

import ss from "./shared-styles";

export default function LandingPage() {
  useEffect(() => {
    const onSignIn = function (googleUser: any) {
      const token = googleUser.getAuthResponse().id_token;
      return $.ajax({
        url: `/api/verify?token=${token}`,
        method: "POST",
      }).then(
        () => {
          // TODO: make a logged in landing page.
          window.location.href = "/plasmids";
        },
        () => {
          (window as any).gapi.auth2.getAuthInstance().signOut();
        }
      );
    };
    (window as any).gapi.signin2.render("g-signin2", {
      scope: "openid email profile",
      longtitle: true,
      prompt: "select_account",
      theme: "dark",
      width: 200,
      onsuccess: onSignIn,
    });
  }, []);
  return (
    <div className={css(styles.container)}>
      <div className={css(styles.box)}>
        <div className={css(styles.title)}>
          Welcome to the {(window as any)._labdbName}!
        </div>
        <div>Please log in.</div>
        <div className={css(styles.prompt)}>
          <div>
            <div id="g-signin2">Sign in with google.</div>
          </div>
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
