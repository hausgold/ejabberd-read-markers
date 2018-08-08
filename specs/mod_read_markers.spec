-xml(rm_ack,
     #elem{name = <<"ack">>,
           xmlns = <<"urn:xmpp:read-markers">>,
           module = hg_read_markers,
           result = {rm_ack, '$id'},
           attrs = [#attr{name = <<"id">>,
                          required = true,
                          dec = {dec_int, [0, infinity]},
                          enc = {enc_int, []}}]}).

-xml(rm_query,
     #elem{name = <<"query">>,
           xmlns = <<"urn:xmpp:read-markers">>,
           module = hg_read_markers,
           result = {rm_query, '$jid'},
           attrs = [#attr{name = <<"jid">>,
                          required = true,
                          dec = {jid, decode, []},
                          enc = {jid, encode, []}}]}).

-xml(rm_unseen_messages,
     #elem{name = <<"unseen-messages">>,
           xmlns = <<"urn:xmpp:read-markers">>,
           module = hg_read_markers,
           result = {rm_unseen_messages, '$amount'},
           attrs = [#attr{name = <<"amount">>,
                          dec = {dec_int, [0, infinity]},
                          enc = {enc_int, []}}]}).

-xml(rm_seen_at,
     #elem{name = <<"seen-at">>,
           xmlns = <<"urn:xmpp:read-markers">>,
           module = hg_read_markers,
           result = '$cdata',
           cdata = #cdata{dec = {dec_utc, []},
                          enc = {enc_utc, []}}}).

-xml(rm_last_message,
     #elem{name = <<"last-message">>,
           xmlns = <<"urn:xmpp:read-markers">>,
           module = hg_read_markers,
           result = {rm_last_message, '$id', '$seen_at'},
           attrs = [#attr{name = <<"id">>,
                          required = true,
                          dec = {dec_int, [0, infinity]},
                          enc = {enc_int, []}}],
           refs = [#ref{name = rm_seen_at,
                        label = '$seen_at',
                        min = 0,
                        max = 1}]}).

%% vim: set filetype=erlang tabstop=2:
