module.exports = (utils) => {
  const match = (xml, regex, message) => {
    if (regex.constructor !== RegExp) {
      regex = new RegExp(regex);
    }

    if (!regex.test(xml)) {
      utils.logError(message, xml, `Match failed for ${regex}`);
    }
  };

  const matchMissing = (xml, regex, message) => {
    if (regex.constructor !== RegExp) {
      regex = new RegExp(regex);
    }

    if (regex.test(xml)) {
      utils.logError(message, xml, `Match for ${regex}`);
    }
  };

  return {
    message: (room) => {
      return (xml, direction) => {
        if (direction !== 'response') { return; }
        const contains = (regex, message) => match(xml, regex, message);
        const missing = (regex, message) => matchMissing(xml, regex, message);

        contains(
          `<message .* from=['"]${room}/admin['"] .*<body>.*</body></message>`,
          `Message response for ${room} failed.`
        );
      };
    },

    receiveMuc: (room, to, expected) => {
      return (xml, direction) => {
        if (direction !== 'response') { return; }
        const contains = (regex, message) => match(xml, regex, message);
        const missing = (regex, message) => matchMissing(xml, regex, message);

        contains(
          /<iq .*xmlns=['"]jabber:client['"]/,
          'Receive IQ response has a bad namespace.'
        );
        contains(
          `<iq .*from=['"]${room}['"]`,
          'Receive IQ response has a bad from attribute.'
        );
        contains(
          `<iq .*to=['"]${to}\/.*['"]`,
          'Receive IQ response user is invalid.'
        );
        contains(
          /<iq .*type=['"]result['"]/,
          'Receive IQ response is not of type >result<.'
        );

        if (expected.amount && expected.amount !== null) {
          contains(
            /<unseen-messages .*xmlns=['"]urn:xmpp:read-markers['"]/,
            'Unseen messages has a bad namespace.'
          );

          contains(
            `<unseen-messages .*amount=['"]${expected.amount}['"].*/>`,
            `The unseen message amount should be ${expected.amount}.`
          );
        }

        if (expected.amount === null) {
          missing(
            `<unseen-messages .*/>`,
            `The unseen-messages element should be missing.`
          );
        }

        if (expected.message && expected.message !== null) {
          contains(
            /<last-message .*xmlns=['"]urn:xmpp:read-markers['"]/,
            'Last message has a bad namespace.'
          );

          contains(
            `<last-message .*id=['"][^'"]+.*/>`,
            `The last message id should not be empty.`
          );

          if (expected.message.id) {
            contains(
              `<last-message .*id=['"]${expected.message.id}['"].*/>`,
              `The last message id should be ${expected.message.id}.`
            );
          }

          if (expected.message.at) {
            contains(
              `<last-message.*><seen-at>.*`,
              `The last message should contain a seen-at element.`
            );

            contains(
              `<last-message.*><seen-at>.*</seen-at>`,
              `The seen-at cdata should not be empty.`
            );

            contains(
              `<last-message.*><seen-at>${expected.message.at}.*Z</seen-at>`,
              `The seen-at cdata is not valid.`
            );
          }
        }

        if (expected.message === null) {
          missing(
            `<last-message .*/>`,
            `The last-message element should be missing.`
          );
        }
      };
    },

    acknowledgeMuc: (room, user) => {
      return (xml, direction) => {
        if (direction !== 'response') { return; }
        const contains = (regex, message) => match(xml, regex, message);
        const missing = (regex, message) => matchMissing(xml, regex, message);

        contains(
          /<iq .*xmlns=['"]jabber:client['"]/,
          'Acknowledge IQ response has a bad namespace.'
        );
        contains(
          `<iq .*from=['"]${room}['"]`,
          'Acknowledge IQ response has a bad from attribute.'
        );
        contains(
          `<iq .*to=['"]${user}\/.*['"]`,
          'Acknowledge IQ response user is invalid.'
        );
        contains(
          /<iq .*type=['"]result['"]/,
          'Acknowledge IQ response is not of type >result<.'
        );
      };
    }
  };
};
