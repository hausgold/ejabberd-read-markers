#!/usr/bin/env node

const async = require('async');

// Setup a new client and run the test suite
require('./src/client')(require('./config'), (client, utils) => {

  // Get the seeds and test cases available
  const seeds = require('./src/seeds')(client);
  const test = require('./src/testcases')(client, utils);

  // Run each test case, sequentially
  async.waterfall([
    seeds,

    // Initial state
    test.receiveMuc('admin', { amount: 0, message: null }),
    test.receiveMuc('alice', { amount: 0, message: null }),
    test.receiveMuc('bob', { amount: 0, message: null }),

    // After a message the initial state changes
    test.message,
    test.receiveMuc('admin', { amount: 0, message: { at: utils.isoHour() } }),
    test.receiveMuc('alice', { amount: 1, message: null }),
    test.receiveMuc('bob', { amount: 1, message: null }),

    // A second message increases the counters
    test.message,
    test.receiveMuc('admin', { amount: 0, message: { at: utils.isoHour() } }),
    test.receiveMuc('alice', { amount: 2, message: null }),
    test.receiveMuc('bob', { amount: 2, message: null }),

    // Alice acknowledges and the counters should be correct
    test.acknowledgeMuc('alice'),
    test.receiveMuc('admin', { amount: 0 }),
    test.receiveMuc('alice', { amount: 0, message: { at: utils.isoHour() } }),
    test.receiveMuc('bob', { amount: 2, message: null }),

    // A third message changes the counters
    test.message,
    test.receiveMuc('admin', { amount: 0, message: { at: utils.isoHour() } }),
    test.receiveMuc('alice', { amount: 1, message: { at: utils.isoHour() } }),
    test.receiveMuc('bob', { amount: 3, message: null }),

    // The last seen message of bob should have the correct id
    test.acknowledgeMuc('bob', '1500000000000001'),
    test.acknowledgeMuc('alice', '1500000000000002'),
    test.receiveMuc('admin', { amount: 0, message: { at: utils.isoHour() } }),
    test.receiveMuc('bob', { amount: 0, message: {
      id: '1500000000000001',
      at: utils.isoHour()
    } }),
    test.receiveMuc('alice', { amount: 0, message: {
      id: '1500000000000002',
      at: utils.isoHour()
    } }),
  ], utils.exit);
});
