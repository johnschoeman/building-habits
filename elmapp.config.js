const ReplaceInFileWebpackPlugin = require("replace-in-file-webpack-plugin");

// This is a hack to get the Url elm package to allow different protocols for
// url schemes. Ideally this support will come to Url but for now this is needed
// to be able to use capacitor, which hosts the web app on capacitor://
// https://github.com/elm/url/issues/10
const replaceProtocolErrorPlugin = new ReplaceInFileWebpackPlugin([
  {
    dir: "build/static",
    rules: [
      {
        search: /var .=.\.fragment/,
        replace: function (match) {
          const varLetter = match[4];
          const fragmentLetter = match[6];
          return (
            "var " +
            varLetter +
            "=" +
            fragmentLetter +
            "?" +
            fragmentLetter +
            ".fragment:{}"
          );
        },
      },
      {
        search:
          'case 1:throw new Error("Browser.application programs cannot handle URLs like this:\\n\\n    "+document.location.href+"\\n\\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.");case 2:',
        replace: "case 2:",
      },
      {
        search: /return (\w+)\((\w+)\.location\.href\)\.(\w+)\s*\|\|\s*\w+\(1\)/,
        replace: function (match, x, y, z) {
          const href = y + ".location.href";
          const toLocalhost =
            "'http://localhost:8080/'+" +
            href +
            ".substring(" +
            href +
            ".indexOf('index.html'))";
          return (
            "return " +
            x +
            "(" +
            href +
            ")." +
            z +
            "||" +
            x +
            "(" +
            toLocalhost +
            ")." +
            z
          );
        },
      },
    ],
  },
]);

module.exports = {
  configureWebpack: (config, _env) => {
    const oldPlugins = config.plugins;
    const newPlugins = [...oldPlugins, replaceProtocolErrorPlugin];
    config.plugins = newPlugins;

    return config;
  },
};
