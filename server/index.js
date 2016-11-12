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
  rooms.joinOrCreate(id, ws);

  ws.on("message", function incoming(msg) {
    console.log("Message: %s", msg);
    rooms.send(id, msg);
  });

  ws.on("close", () => {
    console.log("Disconnect", id);
    rooms.leave(id);
  });
});

server.listen(process.env.PORT, () => {
  console.log("Listening on port", process.env.PORT);
});
