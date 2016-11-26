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
  players[id] = lastRoom;

  if(Object.keys(lastRoom.players).length > 1) {
    rooms[lastRoom.id] = lastRoom;
    lastRoom = null;
  }
};

exports.joinOrCreate = (id, ws) => {
  if(lastRoom) {
    joinLastRoom(id, ws);
    return !lastRoom;
  } else {
    createRoom(id, ws);
    return false;
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
  delete players[id].players[id];
  delete rooms[players[id].id];
  delete players[id];
};

exports.getRoomCount = () => Object.keys(rooms).length;
