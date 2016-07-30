module.exports = {
    entry: "./js/app.js",
    output: {
        path: "./priv/static",
        filename: "app.js",
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
                ],
            },
        }],
    },
    resolve: {
        extensions: ['', '.js', '.jsx'],
    },
};
