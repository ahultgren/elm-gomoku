var width = document.documentElement.clientWidth;
var height = document.documentElement.clientHeight;

var app = Elm.Gomoku.embed(document.getElementById("game"), {
  size: {
    width: width,
    height: height,
  },
  wsAddress: window.Config.wsAddress,
});

window.addEventListener("resize", function (e) {
  if(
    width !== document.documentElement.clientWidth
    || height !== document.documentElement.clientHeight
  ) {
    width = document.documentElement.clientWidth;
    height = document.documentElement.clientHeight;
    app.ports.resizes.send({
      width: width,
      height: height,
    });
  }
}, false);
