-define(MODULE_VERSION, <<"0.15.0-210">>).
-define(NS_READ_MARKERS, <<"urn:xmpp:read-markers">>).

-record(db_entry, {user_jid = 'undefined' :: binary(),
                   room_jid = 'undefined' :: binary(),
                   message_id = 0 :: non_neg_integer(),
                   message_at = undefined :: erlang:timestamp(),
                   unseen = 0 :: non_neg_integer()}).
