module.exports = {
    entry: "./js/app.js",
    output: {
        path: "./public/_s",
        filename: "app_.js",
    },
    module: {
        loaders: [{
            test: /\.jsx?$/,
            exclude: /node_modules/,
            loader: "babel",
            query: {
                presets: ["es2015", "react"],
                plugins: [
                    "array-includes",
                    "syntax-object-rest-spread",
                    "transform-object-rest-spread",
                    "transform-flow-strip-types",
                ],
            },
        }],
    },
    resolve: {
        extensions: ['', '.js', '.jsx'],
    },
};
