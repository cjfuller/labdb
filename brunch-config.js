exports.config = {
  // See http://brunch.io/#documentation for docs.
  files: {
    javascripts: {
      joinTo: {
        "app.js": /^js/,
        "third_party.js": /^jscssdeps/
      }
    },
    stylesheets: {
      joinTo: "app.css"
    }
  },

  conventions: {
    assets: /^assets/,
    ignored: /.*flycheck.*/
  },

  paths: {
    // Dependencies and current project directories to watch
    watched: [
      "js",
      "css",
      "jscssdeps",
    ],

    // Where to compile files to
    public: "public"
  },

  // Configure your plugins
  plugins: {
    babel: {
      // Do not use ES6 compiler in third-party code
      ignore: [/jscssdeps/]
    },
    react: {
      babel: true
    }
  },

  modules: {
    autoRequire: {
      "app.js": ["store"]
    },
    nameCleaner: function(path) {return path.replace(/^jscssdeps\//, "");}
  },

  npm: {
    enabled: true
  }
};
