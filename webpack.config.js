module.exports = {
  entry: "./js/app.ts",
  output: {
    path: __dirname + "/public/_s",
    filename: "app_.js",
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "babel",
            options: {
              presets: ["es2015", "react"],
              plugins: [
                "array-includes",
                "syntax-object-rest-spread",
                "transform-object-rest-spread",
                "transform-flow-strip-types",
              ],
            },
          },
        ],
      },
      {
        test: /\.tsx?$/,
        exclude: /node_modules/,
        use: [{ loader: "ts-loader" }],
      },
    ],
  },
  devtool: "eval-source-map",
  resolve: {
    extensions: [".js", ".jsx", ".ts", ".tsx"],
  },
};
