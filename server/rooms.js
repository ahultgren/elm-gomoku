"use strict";

const uuid = require("uuid");

const players = {};
const rooms = {};
let lastRoom = null;

const createRoom = (id, ws) => {
  lastRoom = {
    id: uuid.v4(),
    players: {
      [id]: ws,
    },
  };

  players[id] = lastRoom;
};

const joinLastRoom = (id, ws) => {
  lastRoom.players[id] = ws;
  rooms[lastRoom.id] = lastRoom;
  players[id] = lastRoom;
  lastRoom = null;
};

exports.joinOrCreate = (id, ws) => {
  if(lastRoom) {
    joinLastRoom(id, ws);
    return "PlayerTwo";
  } else {
    createRoom(id, ws);
    return "PlayerOne";
  }
};

exports.send = (id, msg) => {
  const room = players[id];
  Object.keys(room.players).forEach((maybeOpponentId) => {
    if (maybeOpponentId !== id) {
      room.players[maybeOpponentId].send(msg);
    }
  });
};

exports.leave = (id) => {
  delete rooms[players[id].id];
  delete players[id];
};
