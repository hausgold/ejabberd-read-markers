%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-module(hg_read_markers).

-compile(export_all).

do_decode(<<"last-message">>,
    <<"urn:xmpp:read-markers">>, El, Opts) ->
    decode_rm_last_message(<<"urn:xmpp:read-markers">>,
         Opts, El);
do_decode(<<"seen-at">>, <<"urn:xmpp:read-markers">>,
    El, Opts) ->
    decode_rm_seen_at(<<"urn:xmpp:read-markers">>, Opts,
          El);
do_decode(<<"unseen-messages">>,
    <<"urn:xmpp:read-markers">>, El, Opts) ->
    decode_rm_unseen_messages(<<"urn:xmpp:read-markers">>,
            Opts, El);
do_decode(<<"query">>, <<"urn:xmpp:read-markers">>, El,
    Opts) ->
    decode_rm_query(<<"urn:xmpp:read-markers">>, Opts, El);
do_decode(<<"ack">>, <<"urn:xmpp:read-markers">>, El,
    Opts) ->
    decode_rm_ack(<<"urn:xmpp:read-markers">>, Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"last-message">>, <<"urn:xmpp:read-markers">>},
     {<<"seen-at">>, <<"urn:xmpp:read-markers">>},
     {<<"unseen-messages">>, <<"urn:xmpp:read-markers">>},
     {<<"query">>, <<"urn:xmpp:read-markers">>},
     {<<"ack">>, <<"urn:xmpp:read-markers">>}].

do_encode({rm_ack, _} = Ack, TopXMLNS) ->
    encode_rm_ack(Ack, TopXMLNS);
do_encode({rm_query, _} = Query, TopXMLNS) ->
    encode_rm_query(Query, TopXMLNS);
do_encode({rm_unseen_messages, _} = Unseen_messages,
    TopXMLNS) ->
    encode_rm_unseen_messages(Unseen_messages, TopXMLNS);
do_encode({rm_last_message, _, _} = Last_message,
    TopXMLNS) ->
    encode_rm_last_message(Last_message, TopXMLNS).

do_get_name({rm_ack, _}) -> <<"ack">>;
do_get_name({rm_last_message, _, _}) ->
    <<"last-message">>;
do_get_name({rm_query, _}) -> <<"query">>;
do_get_name({rm_unseen_messages, _}) ->
    <<"unseen-messages">>.

do_get_ns({rm_ack, _}) -> <<"urn:xmpp:read-markers">>;
do_get_ns({rm_last_message, _, _}) ->
    <<"urn:xmpp:read-markers">>;
do_get_ns({rm_query, _}) -> <<"urn:xmpp:read-markers">>;
do_get_ns({rm_unseen_messages, _}) ->
    <<"urn:xmpp:read-markers">>.

pp(rm_ack, 1) -> [id];
pp(rm_query, 1) -> [jid];
pp(rm_unseen_messages, 1) -> [amount];
pp(rm_last_message, 2) -> [id, seen_at];
pp(_, _) -> no.

records() ->
    [{rm_ack, 1}, {rm_query, 1}, {rm_unseen_messages, 1},
     {rm_last_message, 2}].

dec_int(Val, Min, Max) ->
    case erlang:binary_to_integer(Val) of
      Int when Int =< Max, Min == infinity -> Int;
      Int when Int =< Max, Int >= Min -> Int
    end.

dec_utc(Val) -> xmpp_util:decode_timestamp(Val).

enc_int(Int) -> erlang:integer_to_binary(Int).

enc_utc(Val) -> xmpp_util:encode_timestamp(Val).

decode_rm_last_message(__TopXMLNS, __Opts,
           {xmlel, <<"last-message">>, _attrs, _els}) ->
    Seen_at = decode_rm_last_message_els(__TopXMLNS, __Opts,
           _els, undefined),
    Id = decode_rm_last_message_attrs(__TopXMLNS, _attrs,
              undefined),
    {rm_last_message, Id, Seen_at}.

decode_rm_last_message_els(__TopXMLNS, __Opts, [],
         Seen_at) ->
    Seen_at;
decode_rm_last_message_els(__TopXMLNS, __Opts,
         [{xmlel, <<"seen-at">>, _attrs, _} = _el | _els],
         Seen_at) ->
    case xmpp_codec:get_attr(<<"xmlns">>, _attrs,
           __TopXMLNS)
  of
      <<"urn:xmpp:read-markers">> ->
    decode_rm_last_message_els(__TopXMLNS, __Opts, _els,
             decode_rm_seen_at(<<"urn:xmpp:read-markers">>,
                   __Opts, _el));
      _ ->
    decode_rm_last_message_els(__TopXMLNS, __Opts, _els,
             Seen_at)
    end;
decode_rm_last_message_els(__TopXMLNS, __Opts,
         [_ | _els], Seen_at) ->
    decode_rm_last_message_els(__TopXMLNS, __Opts, _els,
             Seen_at).

decode_rm_last_message_attrs(__TopXMLNS,
           [{<<"id">>, _val} | _attrs], _Id) ->
    decode_rm_last_message_attrs(__TopXMLNS, _attrs, _val);
decode_rm_last_message_attrs(__TopXMLNS, [_ | _attrs],
           Id) ->
    decode_rm_last_message_attrs(__TopXMLNS, _attrs, Id);
decode_rm_last_message_attrs(__TopXMLNS, [], Id) ->
    decode_rm_last_message_attr_id(__TopXMLNS, Id).

encode_rm_last_message({rm_last_message, Id, Seen_at},
           __TopXMLNS) ->
    __NewTopXMLNS =
  xmpp_codec:choose_top_xmlns(<<"urn:xmpp:read-markers">>,
            [], __TopXMLNS),
    _els =
  lists:reverse('encode_rm_last_message_$seen_at'(Seen_at,
              __NewTopXMLNS, [])),
    _attrs = encode_rm_last_message_attr_id(Id,
              xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
                       __TopXMLNS)),
    {xmlel, <<"last-message">>, _attrs, _els}.

'encode_rm_last_message_$seen_at'(undefined, __TopXMLNS,
          _acc) ->
    _acc;
'encode_rm_last_message_$seen_at'(Seen_at, __TopXMLNS,
          _acc) ->
    [encode_rm_seen_at(Seen_at, __TopXMLNS) | _acc].

decode_rm_last_message_attr_id(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
      {missing_attr, <<"id">>, <<"last-message">>,
       __TopXMLNS}});
decode_rm_last_message_attr_id(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
    erlang:error({xmpp_codec,
      {bad_attr_value, <<"id">>, <<"last-message">>,
       __TopXMLNS}});
      _res -> _res
    end.

encode_rm_last_message_attr_id(_val, _acc) ->
    [{<<"id">>, enc_int(_val)} | _acc].

decode_rm_seen_at(__TopXMLNS, __Opts,
      {xmlel, <<"seen-at">>, _attrs, _els}) ->
    Cdata = decode_rm_seen_at_els(__TopXMLNS, __Opts, _els,
          <<>>),
    Cdata.

decode_rm_seen_at_els(__TopXMLNS, __Opts, [], Cdata) ->
    decode_rm_seen_at_cdata(__TopXMLNS, Cdata);
decode_rm_seen_at_els(__TopXMLNS, __Opts,
          [{xmlcdata, _data} | _els], Cdata) ->
    decode_rm_seen_at_els(__TopXMLNS, __Opts, _els,
        <<Cdata/binary, _data/binary>>);
decode_rm_seen_at_els(__TopXMLNS, __Opts, [_ | _els],
          Cdata) ->
    decode_rm_seen_at_els(__TopXMLNS, __Opts, _els, Cdata).

encode_rm_seen_at(Cdata, __TopXMLNS) ->
    __NewTopXMLNS =
  xmpp_codec:choose_top_xmlns(<<"urn:xmpp:read-markers">>,
            [], __TopXMLNS),
    _els = encode_rm_seen_at_cdata(Cdata, []),
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
          __TopXMLNS),
    {xmlel, <<"seen-at">>, _attrs, _els}.

decode_rm_seen_at_cdata(__TopXMLNS, <<>>) -> undefined;
decode_rm_seen_at_cdata(__TopXMLNS, _val) ->
    case catch dec_utc(_val) of
      {'EXIT', _} ->
    erlang:error({xmpp_codec,
      {bad_cdata_value, <<>>, <<"seen-at">>, __TopXMLNS}});
      _res -> _res
    end.

encode_rm_seen_at_cdata(undefined, _acc) -> _acc;
encode_rm_seen_at_cdata(_val, _acc) ->
    [{xmlcdata, enc_utc(_val)} | _acc].

decode_rm_unseen_messages(__TopXMLNS, __Opts,
        {xmlel, <<"unseen-messages">>, _attrs, _els}) ->
    Amount = decode_rm_unseen_messages_attrs(__TopXMLNS,
               _attrs, undefined),
    {rm_unseen_messages, Amount}.

decode_rm_unseen_messages_attrs(__TopXMLNS,
        [{<<"amount">>, _val} | _attrs], _Amount) ->
    decode_rm_unseen_messages_attrs(__TopXMLNS, _attrs,
            _val);
decode_rm_unseen_messages_attrs(__TopXMLNS,
        [_ | _attrs], Amount) ->
    decode_rm_unseen_messages_attrs(__TopXMLNS, _attrs,
            Amount);
decode_rm_unseen_messages_attrs(__TopXMLNS, [],
        Amount) ->
    decode_rm_unseen_messages_attr_amount(__TopXMLNS,
            Amount).

encode_rm_unseen_messages({rm_unseen_messages, Amount},
        __TopXMLNS) ->
    __NewTopXMLNS =
  xmpp_codec:choose_top_xmlns(<<"urn:xmpp:read-markers">>,
            [], __TopXMLNS),
    _els = [],
    _attrs = encode_rm_unseen_messages_attr_amount(Amount,
               xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
                        __TopXMLNS)),
    {xmlel, <<"unseen-messages">>, _attrs, _els}.

decode_rm_unseen_messages_attr_amount(__TopXMLNS,
              undefined) ->
    undefined;
decode_rm_unseen_messages_attr_amount(__TopXMLNS,
              _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
    erlang:error({xmpp_codec,
      {bad_attr_value, <<"amount">>, <<"unseen-messages">>,
       __TopXMLNS}});
      _res -> _res
    end.

encode_rm_unseen_messages_attr_amount(undefined,
              _acc) ->
    _acc;
encode_rm_unseen_messages_attr_amount(_val, _acc) ->
    [{<<"amount">>, enc_int(_val)} | _acc].

decode_rm_query(__TopXMLNS, __Opts,
    {xmlel, <<"query">>, _attrs, _els}) ->
    Jid = decode_rm_query_attrs(__TopXMLNS, _attrs,
        undefined),
    {rm_query, Jid}.

decode_rm_query_attrs(__TopXMLNS,
          [{<<"jid">>, _val} | _attrs], _Jid) ->
    decode_rm_query_attrs(__TopXMLNS, _attrs, _val);
decode_rm_query_attrs(__TopXMLNS, [_ | _attrs], Jid) ->
    decode_rm_query_attrs(__TopXMLNS, _attrs, Jid);
decode_rm_query_attrs(__TopXMLNS, [], Jid) ->
    decode_rm_query_attr_jid(__TopXMLNS, Jid).

encode_rm_query({rm_query, Jid}, __TopXMLNS) ->
    __NewTopXMLNS =
  xmpp_codec:choose_top_xmlns(<<"urn:xmpp:read-markers">>,
            [], __TopXMLNS),
    _els = [],
    _attrs = encode_rm_query_attr_jid(Jid,
              xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
                 __TopXMLNS)),
    {xmlel, <<"query">>, _attrs, _els}.

decode_rm_query_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
      {missing_attr, <<"jid">>, <<"query">>, __TopXMLNS}});
decode_rm_query_attr_jid(__TopXMLNS, _val) ->
    case catch jid:decode(_val) of
      {'EXIT', _} ->
    erlang:error({xmpp_codec,
      {bad_attr_value, <<"jid">>, <<"query">>, __TopXMLNS}});
      _res -> _res
    end.

encode_rm_query_attr_jid(_val, _acc) ->
    [{<<"jid">>, jid:encode(_val)} | _acc].

decode_rm_ack(__TopXMLNS, __Opts,
        {xmlel, <<"ack">>, _attrs, _els}) ->
    Id = decode_rm_ack_attrs(__TopXMLNS, _attrs, undefined),
    {rm_ack, Id}.

decode_rm_ack_attrs(__TopXMLNS,
        [{<<"id">>, _val} | _attrs], _Id) ->
    decode_rm_ack_attrs(__TopXMLNS, _attrs, _val);
decode_rm_ack_attrs(__TopXMLNS, [_ | _attrs], Id) ->
    decode_rm_ack_attrs(__TopXMLNS, _attrs, Id);
decode_rm_ack_attrs(__TopXMLNS, [], Id) ->
    decode_rm_ack_attr_id(__TopXMLNS, Id).

encode_rm_ack({rm_ack, Id}, __TopXMLNS) ->
    __NewTopXMLNS =
  xmpp_codec:choose_top_xmlns(<<"urn:xmpp:read-markers">>,
            [], __TopXMLNS),
    _els = [],
    _attrs = encode_rm_ack_attr_id(Id,
           xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
                    __TopXMLNS)),
    {xmlel, <<"ack">>, _attrs, _els}.

decode_rm_ack_attr_id(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
      {missing_attr, <<"id">>, <<"ack">>, __TopXMLNS}});
decode_rm_ack_attr_id(__TopXMLNS, _val) ->
    case catch dec_int(_val, 0, infinity) of
      {'EXIT', _} ->
    erlang:error({xmpp_codec,
      {bad_attr_value, <<"id">>, <<"ack">>, __TopXMLNS}});
      _res -> _res
    end.

encode_rm_ack_attr_id(_val, _acc) ->
    [{<<"id">>, enc_int(_val)} | _acc].
