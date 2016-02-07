const React = require("react");
const {StyleSheet, css} = require("aphrodite");
const $ = require("jquery");

const ss = require("./shared-styles.js");

const LandingPage = React.createClass({
    componentDidMount: function() {
        const onSignIn = function(googleUser) {
            $.ajax({
                url: `/api/verify?token=${googleUser.getAuthResponse().id_token}`,
                method: "POST",
            }).then(() => {
                window.location.reload;
            });
        };
        window.gapi.signin2.render('g-signin2', {
            scope: 'openid email profile',
            longtitle: true,
            theme: 'dark',
            width: 200,
            onsuccess: onSignIn,
            onfailure: window.location.reload,
        });
    },
    render: function() {
        return <div className={css(styles.container)}>
            <div className={css(styles.box)}>
                <div className={css(styles.title)}>
                    Welcome to the {window._labdbName}!
                </div>
                <div>
                    Please log in.
                </div>
                <div className={css(styles.prompt)}>
                    <div className={css(styles.loginButton)}>
                        <div id="g-signin2">Sign in with google.</div>
                    </div>
                </div>
            </div>
        </div>;
    },
});

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

module.exports = LandingPage;
