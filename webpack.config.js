module.exports = {
    entry: "./js/app.js",
    output: {
        path: "./public/_s",
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
                    "syntax-object-rest-spread",
                    "transform-object-rest-spread",
                ],
            },
        }],
    },
    resolve: {
        alias: {
            aphrodite: "node_modules/aprhodite/dist/aprhodite.js",
        },
        extensions: ['', '.js', '.jsx'],
    },
};