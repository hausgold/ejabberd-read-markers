const request = require('request');
const async = require('async');
const config = require('../config');
const utils = require('./utils')(config);

/**
 * Create a new ejabberd user account.
 *
 * @param {String} name The name of the user
 * @param {Function} callback The function to call on finish
 */
var createUser = function(name, callback)
{
  request.post(
    `http://${config.hostname}/admin/server/${config.hostname}/users/`,
    {
      auth: {
        user: config.jid,
        pass: config.password
      },
      form: {
        newusername: name,
        newuserpassword: name,
        addnewuser: 'add'
      }
    },
    function(err, res, body) {
      if (!err && res.statusCode === 200) {
        let jid = `${name}@${config.hostname}`;
        utils.log(`Create user ${jid.blue}`, false, 1);
        return callback && callback(null, {
          user: name,
          password: name,
          jid: jid
        });
      }

      utils.log(`User creation failed. (${name})`, false, 1);
      utils.log(`Error: ${err.message}`, false, 1);
      callback && callback(new Error());
  });
};

/**
 * Create all given users and pass them back as an array of
 * user objects.
 *
 * @param {Array} users An array of user names
 * @param {Function} callback The function to call on finish
 */
module.exports = function(users, callback)
{
  async.map(users, createUser, function(err, users) {
    if (err) {
      return callback && callback(err);
    }

    callback && callback(null, users);
  });
};
