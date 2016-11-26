const config = require("../config");

module.exports = (state) => {
  return `
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, minimum-scale=1">
  <title>Gomoku</title>
  <link rel="stylesheet" href="/css/main.css">
</head>
<body>
  <div id="game" class="game"></div>
  <script src="/dist/elm.js"></script>
  <script>
    var Config = {
      wsAddress: "ws://${config.get("HOSTNAME")}",
      gameCount: ${state.gameCount},
    };
  </script>
  <script src="/js/main.js"></script>
  <script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

    ga('create', '${config.get("GA_CODE")}', 'auto');
    ga('send', 'pageview');

  </script>
</body>
</html>`;
};
