const faker = require('faker');
const setupClient = require('./client');

module.exports = (client, utils) => {
  // Setup a reference shortcut
  var config = client.config;

  // Setup the stanza validator
  const validator = require('./stanza-validator')(utils);

  return {
    // Send message to room
    message: (callback) => {
      utils.log('Send a new message to ' + config.room.blue);
      client.joinRoom(config.room, 'admin');
      setTimeout(() => {
        let message = faker.hacker.phrase();
        utils.setMatcher(message, validator.message(config.room));
        client.sendMessage({
          to: config.room,
          body: message,
          type: 'groupchat'
        });
        setTimeout(callback, 1000);
      }, 200);
    },

    // Ask for the last read message
    receiveMuc: (user, expected) => {
      if (!user) { user = config.jid; }
      else { user = `${user}@${config.hostname}`; }

      return (callback) => {
        utils.restoreMatcher();
        utils.setMatcherCallback(
          validator.receiveMuc(config.room, config.jid, expected)
        );
        utils.log('Ask for the last read message of ' + user.blue);
        client.sendIq({
          type: 'get',
          to: config.room,
          query: { jid: user }
        }, () => { setTimeout(callback, 1000); });
      };
    },

    // Ack the last read message
    acknowledgeMuc: (username, id = '1500000000000000') => {
      if (!username) {
        var user = config.jid;
        var password = config.password;
      } else {
        var user = `${username}@${config.hostname}`;
        var password = username;
      }

      return (callback) => {
        let newConf = Object.assign({}, client.config, {
          jid: user,
          password: password
        });
        delete newConf.credentials;

        setupClient(newConf, (client, utils) => {
          utils.restoreMatcher();
          utils.setMatcherCallback(
            validator.acknowledgeMuc(config.room, user)
          );
          utils.log('Acknowledge the last read message of ' + user.blue);
          client.sendIq({
            type: 'set',
            to: config.room,
            ack: { id: id }
          }, () => { setTimeout(callback, 1000); });
        });
      };
    }
  };
};
