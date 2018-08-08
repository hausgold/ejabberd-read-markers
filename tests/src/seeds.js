const async = require('async');
const createUsers = require('../lib/users');
const createRoom = require('../lib/rooms');

module.exports = (client) => {
  return (callback) => {
    async.waterfall([
      function(callback) { createUsers(client.config.users, callback); },
      function(users, callback) {
        createRoom(client.config.room,
                   users.map((user) => user.jid),
                   client,
                   callback);
      }
    ], () => callback());
  };
};
