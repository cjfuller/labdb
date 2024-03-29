import type { CSSProperties } from "aphrodite";

const colors = {
  borderColor: "#e3e3e3",
  darkGray: "rgba(0, 0, 0, 0.2)",
  // Synced to radix grass 9
  labdbGreen: "#46A758",
  labdbGreenLight: "#66aa66",
  lightBackground: "#f8f8f8",
  mediumBackground: "#e0e0e0",
  bitDarkBackground: "#c0c0c0",
  // Synced to radix cyan 7
  linkBaseColor: "#12677e",
  // Synced to radix lime 7
  linkHoverColor: "#496231",
  mutedBlue: "#115599",
  mutedBlueSemitransparent: "rgba(17, 85, 153, 0.3)",
  mutedBlueMoreOpaque: "rgba(17, 85, 153, 0.8)",
  yesGreen: "#7aa141",
  noRed: "#ff3333",
  ambiguousBlue: "#00aaff",
};

// TODO: use aprhodite font face support
const fonts = {
  monospace: "Source Code Pro, monospace",
  base: "Lato, sans-serif",
  content: "Lato, sans-serif",
  contrast: "Montserrat, sans-serif",
  weights: {
    emph: 600,
  },
};

const sizes = {
  buttonHeightPx: 40,
  cornerRadiusPx: 3,
  fontSizeExtraLarge: 20,
  fontSizeLarge: 17,
  fontSizeMedium: 14,
  fontSizeNormal: 13,
  fontSizeCaption: 11,
  hamburgerWidthPx: 50,
  navbarHeightPx: 50,
  paddingPx: 10,
};

const elements: { [s: string]: CSSProperties } = {
  link: {
    color: colors.linkBaseColor,
    ":hover": {
      color: colors.linkHoverColor,
      textDecoration: "underline",
    },
    textDecoration: "none",
  },
  sectionContents: {
    backgroundColor: colors.lightBackground,
    borderLeft: `3px solid ${colors.mutedBlue}`,
    borderRadius: sizes.cornerRadiusPx,
    fontFamily: fonts.content,
    fontSize: sizes.fontSizeMedium,
    minHeight: 2 * sizes.paddingPx,
    padding: sizes.paddingPx,
  },
  sectionLabel: {
    fontFamily: fonts.content,
    fontSize: sizes.fontSizeLarge,
  },
  inputField: {
    background: "inherit",
    border: "none",
    display: "inline-block",
    fontFamily: fonts.base,
    fontSize: sizes.fontSizeNormal,
    height: "inherit",
    marginBottom: "inherit",
    maxWidth: "100%",
    width: "100%",
  },
  fieldName: {
    display: "inline-block",
    fontWeight: fonts.weights.emph as 600,
    marginRight: sizes.paddingPx,
  },
};

const traits = {
  editableBorders: {
    border: "none",
    borderBottom: `1px solid ${colors.borderColor}`,
    borderRadius: 0,
  },
  editableFocus: {
    ":focus": {
      borderBottom: "1px solid black",
      outline: "none",
    },
  },
  shadowedButton: {
    boxShadow: `1px 1px 1px ${colors.bitDarkBackground}`,
    ":active": {
      boxShadow: `inset 1px 1px 2px 1px ${colors.darkGray}`,
    },
  },
  shadowed: {
    boxShadow: `0px 0px 10px 1px ${colors.darkGray}`,
  },
};

export default { colors, elements, fonts, sizes, traits };
