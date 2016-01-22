"use strict";

const colors = {
    borderColor: "#e3e3e3",
    labdbGreen: "#559955",
    lightBackground: "#f8f8f8",
    mediumBackground: "#e0e0e0",
    bitDarkBackground: "#c0c0c0",
    linkBaseColor: "#007799",
    linkHoverColor: "#009977",

};

const elements = {
    link: {
        color: colors.linkBaseColor,
        ':hover': {
            color: colors.linkHoverColor,
            textDecoration: "underline",
        },
        textDecoration: "none",
    },
};

// TODO: use aprhodite font face support
const fonts = {
    monospace: "Source Code Pro, monospace",
    base: "Open Sans, sans-serif",
    content: "Open Sans, sans-serif",
    contrast: "Montserrat, sans-serif",
};


const sizes = {
    buttonHeightPx: 40,
    cornerRadiusPx: 3,
    fontSizeLarge: 16,
    fontSizeMedium: 13,
    hamburgerWidthPx: 50,
    navbarHeightPx: 50,
    paddingPx: 10,
};

module.exports = {colors, elements, fonts, sizes};

