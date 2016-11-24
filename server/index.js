"use strict";

const server = require("http").createServer();
const WebSocketServer = require("ws").Server;
const uuid = require("uuid");
const express = require("express");
const config = require("../config");
const rooms = require("./rooms");
const mainView = require("./main-view");

const port = config.get("PORT");
const app = express();
const wss = new WebSocketServer({
  server,
});

app.get("/", (req, res, next) => {
  res.send(mainView());
});
app.use(express.static("public"));

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
    rooms.send(id, JSON.stringify({
      type: "Disconnected",
    }));
    rooms.leave(id);
  });

  ws.on("error", (e) => {
    console.log("ws error", e);
  });
});

wss.on("error", (e) => {
  console.log("wss error", e);
});

server.on("request", app);
server.listen(port, () => {
  console.log("Listening on port", port);
});
