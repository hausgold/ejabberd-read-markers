// Here are the XMPP specifications for our custom read markers module.
// Require this file and pass in a +stanza.io+ client instance. The code
// below takes care to register the new stanzas.
module.exports = (client) => {
  const jxt = client.stanzas;
  const Iq = jxt.getDefinition('iq', 'jabber:client');

  const AckReadMarkers = jxt.define({
    name: 'ack',
    element: 'ack',
    namespace: 'urn:xmpp:read-markers',
    fields: {
      id: jxt.utils.attribute('id')
    }
  });
  jxt.extend(Iq, AckReadMarkers);

  const QueryReadMarkers = jxt.define({
    name: 'query',
    element: 'query',
    namespace: 'urn:xmpp:read-markers',
    fields: {
      jid: jxt.utils.attribute('jid')
    }
  });
  jxt.extend(Iq, QueryReadMarkers);
};
