"use strict";

const server = require("http").createServer();
const WebSocketServer = require("ws").Server;
const uuid = require("uuid");
const rooms = require("./rooms");

const wss = new WebSocketServer({
  server,
});

wss.on("connection", function connection(ws) {
  const id = uuid.v4();

  console.log("Connect", id);

  if(rooms.joinOrCreate(id, ws)) {
    rooms.send(id, JSON.stringify({
      type: "Start",
      player: "PlayerOne",
    }));

    ws.send(JSON.stringify({
      type: "Start",
      player: "PlayerTwo",
    }));
  }

  ws.on("message", function incoming(msg) {
    console.log("Message: %s", msg);
    rooms.send(id, msg);
  });

  ws.on("close", () => {
    console.log("Disconnect", id);
    rooms.leave(id);
  });

  ws.on("error", (e) => {
    console.log("ws error", e);
  });
});

wss.on("error", (e) => {
  console.log("wss error", e);
});

server.listen(process.env.PORT, () => {
  console.log("Listening on port", process.env.PORT);
});
