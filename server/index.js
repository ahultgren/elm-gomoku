"use strict";

const server = require("http").createServer();
const WebSocketServer = require("ws").Server;

const wss = new WebSocketServer({
  server,
});

const game = {
  playerOne: {},
  playerTwo: {},
};

wss.on("connection", function connection(ws) {
  console.log("Connection");

  let self;
  let opponent;
  if(!game.playerOne.ws) {
    game.playerOne.ws = ws;
    self = game.playerOne;
    opponent = game.playerTwo;
  } else {
    game.playerTwo.ws = ws;
    self = game.playerTwo;
    opponent = game.playerOne;
  }

  ws.on("message", function incoming(message) {
    console.log("received: %s", message);
    if(opponent.ws) {
      console.log("Sending to opponent");
      opponent.ws.send(message);
    }
  });

  ws.on("close", () => {
    console.log("Disconnected");
    self.ws = undefined;
  });

  ws.send("connected");
});

server.listen(process.env.PORT, () => {
  console.log("Listening on port", process.env.PORT);
});
