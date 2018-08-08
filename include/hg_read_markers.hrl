%% This file was generated automatically by compile-xmpp-specs

-record(rm_query, {jid :: jid:jid()}).
-type rm_query() :: #rm_query{}.

-record(rm_ack, {id :: non_neg_integer()}).
-type rm_ack() :: #rm_ack{}.

-record(rm_unseen_messages, {amount :: 'undefined' | non_neg_integer()}).
-type rm_unseen_messages() :: #rm_unseen_messages{}.

-record(rm_last_message, {id :: non_neg_integer(),
                          seen_at :: undefined | erlang:timestamp()}).
-type rm_last_message() :: #rm_last_message{}.
