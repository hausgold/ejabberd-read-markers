-module(mod_read_markers).
-author("hermann.mayer92@gmail.com").
-behaviour(gen_mod).
-export([%% ejabberd module API
         start/2, stop/1, reload/3, mod_opt_type/1, depends/2,
         %% Database
         get_last/2, store_last/3, increment_unseen/2,
         %% Hooks
         on_muc_iq/2, on_muc_message/3
        ]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-include("mod_muc_room.hrl").
-include("hg_read_markers.hrl").
-include("mod_read_markers.hrl").

-callback init(binary(), gen_mod:opts())
  -> ok | {ok, pid()}.
-callback get_last(binary(), binary(), binary())
  -> {acked, #db_entry{}} | {unacked, #db_entry{}} | not_found | {error, any()}.
-callback store_last(binary(), binary(), binary(), non_neg_integer())
  -> ok | {error, any()}.
-callback increment_unseen(binary(), binary(), binary())
  -> any().

%% Start the module by implementing the +gen_mod+ behaviour. Here we register
%% the custom XMPP codec, the IQ handler and the hooks to listen to, for the
%% custom read markers functionality.
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, Opts) ->
  %% Initialize the database module
  Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
  Mod:init(Host, Opts),
  %% Register the custom XMPP codec
  xmpp:register_codec(hg_read_markers),
  %% Register hooks
  %% Run the MUC IQ hook after mod_mam (50)
  ejabberd_hooks:add(muc_process_iq, Host, ?MODULE, on_muc_iq, 51),
  %% Run the MUC message hook after mod_mam (50)
  ejabberd_hooks:add(muc_filter_message, Host, ?MODULE, on_muc_message, 51),
  %% Log the boot up
  ?INFO_MSG("[RM] Start read markers (v~s) for ~s", [?MODULE_VERSION, Host]),
  ok.

%% Stop the module, and deregister the XMPP codec and all hooks as well as the
%% IQ handler.
-spec stop(binary()) -> any().
stop(Host) ->
  %% Deregister the custom XMPP codec
  xmpp:unregister_codec(hg_read_markers),
  %% Deregister all the hooks
  ejabberd_hooks:delete(muc_process_iq, Host, ?MODULE, on_muc_iq, 51),
  ejabberd_hooks:delete(muc_filter_message, Host, ?MODULE, on_muc_message, 51),
  ?INFO_MSG("[RM] Stop read markers", []),
  ok.

%% Inline reload the module in case of external triggered +ejabberdctl+ reloads.
-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(Host, NewOpts, OldOpts) ->
  %% Reload the custom XMPP codec
  xmpp:register_codec(hg_read_markers),
  %% Reload the database module on changes
  NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
  OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
  if NewMod /= OldMod -> NewMod:init(Host, NewOpts);
    true -> ok
  end,
  ok.

%% This function matches acknowledgement requests and writes them to the
%% database in order to persist the last read message of a user on a given MUC.
-spec on_muc_iq(ignore | iq(), mod_muc_room:state()) -> ignore | iq().
on_muc_iq(#iq{type = set, from = User, to = Room,
              sub_els = [#rm_ack{id = Id}]} = IQ, _MUCState) ->
  log_ack(Room, User, Id),
  store_last(Room, User, Id),
  xmpp:make_iq_result(IQ);

%% This function matches requests for the last read message of a given user on
%% a given MUC. We lookup the combination on the database and pass back the
%% data. Simple as cake.
on_muc_iq(#iq{type = get, to = Room, lang = Lang,
              sub_els = [#rm_query{jid = User}]} = IQ, _MUCState) ->
  log_receive(Room, User),
  case get_last(Room, User) of
    {acked, #db_entry{message_id = Id, message_at = At, unseen = Unseen}} ->
      make_iq_result_els(IQ, [#rm_last_message{id = Id, seen_at = At},
                              #rm_unseen_messages{amount = Unseen}]);
    {unacked, #db_entry{unseen = Unseen}} ->
      xmpp:make_iq_result(IQ, #rm_unseen_messages{amount = Unseen});
    not_found ->
      xmpp:make_iq_result(IQ, #rm_unseen_messages{amount = 0});
    _ ->
      Error = xmpp:err_internal_server_error(<<"Database failure">>, Lang),
      xmpp:make_error(IQ, Error)
  end;

%% Match all MUC IQ's and pass them back, unmodified.
on_muc_iq(IQ, _MUCState) -> IQ.

%% Listen to MUC messages and acknowledge the sender last read message and
%% increment the unseen counter for all room members.
-spec on_muc_message(message(), mod_muc_room:state(), binary()) -> message().
on_muc_message(#message{from = Sender, meta = Meta} = Packet,
               #state{jid = Room, affiliations = Affiliations},
               _FromNick) ->
  %% Extract and prepare the message details which are required to increment
  %% the unseen messages for all room members (except the sender) and to
  %% acknowledge the senders last read message.
  Id = maps:get(stanza_id, Meta),
  %% Acknowledge the message to be seen by the sender. It's fine to assume the
  %% sender saw his very own message.
  store_last(Room, Sender, Id),
  %% Filter all members, except the sender for an increment of their unseen
  %% messages for the message room.
  Members = maps:remove(bare_jid(Sender),
                        affiliations_to_jid_list(Affiliations)),
  %% Increment the unseen message counter for all room members.
  maps:fold(fun(_BareJid, User, ok) -> increment_unseen(Room, User) end,
            ok, Members),
  %% We do not filter, we listen only.
  Packet;

%% Match all MUC messages and pass back the unmodified packet.
on_muc_message(Packet, _MUCState, _FromNick) -> Packet.

%% This function is dedicated to read a database record of the last read
%% message for a given user/room combination. When the lookup fails we deliver
%% no last message details, but an zero count for unseen messages.
-spec get_last(jid(), jid()) -> any().
get_last(#jid{} = Room, #jid{lserver = LServer} = User) ->
  Mod = gen_mod:db_mod(LServer, ?MODULE),
  Mod:get_last(LServer, bare_jid(Room), bare_jid(User)).

%% This function writes a new row to the last read messages database in order
%% to persist the acknowledgement.
-spec store_last(jid(), jid(), non_neg_integer()) -> any().
store_last(#jid{} = Room, #jid{lserver = LServer} = User, Id) ->
  Mod = gen_mod:db_mod(LServer, ?MODULE),
  Mod:store_last(LServer, bare_jid(Room), bare_jid(User), Id).

%% This function increments the last unseen message counter for the given
%% user/room combination. In case there is no read message record yet, we
%% create a new one and increment the unseen counter afterwards.
-spec increment_unseen(jid(), jid()) -> any().
increment_unseen(#jid{} = Room, #jid{lserver = LServer} = User) ->
  Mod = gen_mod:db_mod(LServer, ?MODULE),
  Mod:increment_unseen(LServer, bare_jid(Room), bare_jid(User)).

%% Logs when we see a read message ack request.
-spec log_ack(jid(), jid(), non_neg_integer()) -> any().
log_ack(#jid{} = Room, #jid{} = User, Id) ->
  ?DEBUG("[RM][Ack] Room: ~s, User: ~s, Id: ~p",
          [bare_jid(Room), bare_jid(User), Id]).

%% Logs when we see a read message receive request.
-spec log_receive(jid(), jid()) -> any().
log_receive(#jid{} = Room, #jid{} = User) ->
  ?DEBUG("[RM][Receive] Room: ~s, User: ~s",
          [bare_jid(Room), bare_jid(User)]).

%% Allow IQ results to have multiple sub elements.
%% See: http://bit.ly/2KgmAQb
-spec make_iq_result_els(iq(), [xmpp_element() | xmlel() | undefined]) -> iq().
make_iq_result_els(#iq{from = From, to = To} = IQ, SubEls) ->
  IQ#iq{type = result, to = From, from = To, sub_els = SubEls}.

%% Convert the given JID (full, or bare) to a bare JID and encode it to a
%% string.
-spec bare_jid(jid()) -> binary().
bare_jid(#jid{} = Jid) -> jid:encode(jid:remove_resource(Jid)).

%% Convert the given affiliation dictionary to a map of bare JID's (binary) as
%% keys and their corresponding +jid()+ records.
-spec affiliations_to_jid_list(?TDICT) -> map().
affiliations_to_jid_list(Dict) ->
  lists:foldl(fun({{User, Host, Resource}, _Aff}, Map) ->
                Jid = jid:make(User, Host, Resource),
                maps:put(bare_jid(Jid), Jid, Map)
              end, #{}, dict:to_list(Dict)).

%% Some ejabberd custom module API fullfilments
depends(_Host, _Opts) -> [{mod_muc, hard}].

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
%% TODO: http://bit.ly/2LU3jto
%% mod_opt_type(_) -> [db_type].
mod_opt_type(_) -> [].
