module.exports = {
  config: {
    paths: {
      watched: ["app"]
    },

    files: {
      javascripts: {
        joinTo: "js/app.js"
      },
      stylesheets: {
        joinTo: "css/app.css"
      }
    },

    plugins: {
      elmBrunch: {
        mainModules: ["app/elm/Main.elm"],
        outputFolder: "app/js/",
        outputFile: "elm.js"
      },
      sass: {
        mode: "native"
      }
    },

    notifications: false
  }
};
