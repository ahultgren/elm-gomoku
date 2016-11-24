const config = require("../config");

module.exports = () => {
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
    };
  </script>
  <script src="/js/main.js"></script>
</body>
</html>`;
};
