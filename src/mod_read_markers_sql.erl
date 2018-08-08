-module(mod_read_markers_sql).
-author("hermann.mayer92@gmail.com").
-behaviour(mod_read_markers).
-compile([{parse_transform, ejabberd_sql_pt}]).
-export([init/2, get_last/3, store_last/4, increment_unseen/3]).

-include("mod_read_markers.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("ejabberd_sql_pt.hrl").

init(_Host, _Opts) ->
  ok.

%% This function is dedicated to read a database record of the last read
%% message for a given user/room combination. When the lookup fails we deliver
%% no last message details, but an zero count for unseen messages.
-spec get_last(binary(), binary(), binary()) -> any().
get_last(LServer, RoomJid, UserJid) ->
  Query = ?SQL("SELECT @(last_message_id)d, "
               "@(last_message_at)d, @(unseen_messages)d "
               "FROM read_messages "
               "WHERE user_jid=%(UserJid)s "
               "AND room_jid=%(RoomJid)s"),
  case ejabberd_sql:sql_query(LServer, Query) of
    %% An unacknowledged read message was found
    {selected, [{0, 0, Unseen}]} ->
      {unacked, #db_entry{user_jid = UserJid,
                          room_jid = RoomJid,
                          unseen = Unseen}};
    %% An acknowledged read message was found
    {selected, [{Id, Timestamp, Unseen}]} ->
      At = integer_to_timestamp(Timestamp),
      {acked, #db_entry{user_jid = UserJid,
                        room_jid = RoomJid,
                        message_id = Id,
                        message_at = At,
                        unseen = Unseen}};
    %% We see database issues or no row was found
    {'EXIT', _Reason} -> {error, db_fail};
    _ -> not_found
  end.

%% This function writes a new row to the last read messages database in order
%% to persist the acknowledgement.
-spec store_last(binary(), binary(), binary(), non_neg_integer()) -> any().
store_last(LServer, RoomJid, UserJid, Id) ->
  if
    Id == 0 -> Now = 0;
    Id > 0 ->
      Now = p1_time_compat:system_time(micro_seconds),
      ?DEBUG("[RM][Record] Acknowledge (~p) of ~s on ~s",
             [Id, UserJid, RoomJid])
  end,
  case ?SQL_UPSERT(LServer,
                   "read_messages",
                   ["!user_jid=%(UserJid)s",
                    "!room_jid=%(RoomJid)s",
                    "last_message_id=%(Id)d",
                    "last_message_at=%(Now)d",
                    "unseen_messages=0"]) of
  ok -> ok;
  _Err -> {error, db_failure}
  end.

%% This function increments the last unseen message counter for the given
%% user/room combination. In case there is no read message record yet, we
%% create a new one and increment the unseen counter afterwards.
-spec increment_unseen(binary(), binary(), binary()) -> any().
increment_unseen(LServer, RoomJid, UserJid) ->
  Result = get_last(LServer, RoomJid, UserJid),
  case Result of
    %% In case now read message record was found, we create a new one and
    %% increment the unseen messages counter accordingly. This will result in
    %% +1+ as new unseen counter value. (We call our self for this)
    not_found ->
      store_last(LServer, RoomJid, UserJid, 0),
      increment_unseen(LServer, RoomJid, UserJid);
    %% When we found a read message record, no matter if acknowledged or not we
    %% increment the unseen counter.
    {acked, #db_entry{} = Row} -> increment_unseen_row(LServer, Row);
    {unacked, #db_entry{} = Row} -> increment_unseen_row(LServer, Row);
    %% In case of database failures we just stop doing anything.
    _ -> ok
  end.

%% Increment the unseen counter on an actual read message record.
-spec increment_unseen_row(binary(), #db_entry{}) -> any().
increment_unseen_row(LServer, #db_entry{user_jid = UserJid,
                                        room_jid = RoomJid,
                                        unseen = Unseen}) ->
  NewUnseen = Unseen + 1,
  ?DEBUG("[RM][Record] Increment unseen messages (~p -> ~p) of ~s on ~s",
         [Unseen, NewUnseen, UserJid, RoomJid]),
  ?SQL_UPSERT(LServer, "read_messages", ["!user_jid=%(UserJid)s",
                                         "!room_jid=%(RoomJid)s",
                                         "unseen_messages=%(NewUnseen)d"]).

%% Convert the given integer to an Erlang timestamp. We assume the given
%% timestamp is in micro seconds.
-spec integer_to_timestamp(non_neg_integer()) -> erlang:timestamp().
integer_to_timestamp(Int) ->
  {Int div 1000000000000, Int div 1000000 rem 1000000, Int rem 100000}.
