erlyfix
=====

[![Travis](https://img.shields.io/travis/basiliscos/erl-erlyfix.svg)](https://travis-ci.org/basiliscos/erl-erlyfix)
[![license](https://img.shields.io/github/license/basiliscos/erl-erlyfix.svg)](https://github.com/basiliscos/erl-erlyfix/blob/master/LICENSE)

FIX (Foreign Information eXchange) protocol implementation in erlang.

Versions
-----

Versions supported: R18 and up

Description
-----

This FIX protocol implementation based on XML FIX-protocol definitions provided by [quickfixengine.org](http://quickfixengine.org/). It is possible to have own/proprietary extenstion XML, which extends the quickfix's definition. 

The library provides only serialization, deserialization and basic validation of FIX-messages and does not provides network layer. You have to build your own FIX-client/server. 

Synopsis
-----

```erlang
% loading protocol
{ok, Protocol} = erlyfix_protocol:load("test/FIX44.xml"),
{ok, Protocol} = erlyfix_protocol:load("test/FIX44.xml", "test/extension-sample.xml"),

% message serialization
{ok, IoList} = erlyfix_protocol:serialize(Protocol, 'MarketDataSnapshotFullRefresh', [
    {'SenderCompID', <<"me">>},                         % field
    {'TargetCompID', <<"you">>},
    {'MsgSeqNum', 1},
    {'SendingTime', <<"20171109-16:19:07.541">>},
    {'Instrument', [{'Symbol', <<"EURCHF">>}] },        % component
    {'MDReqID', <<"31955:1510225047.01637:EURCHF">>},
    {'MDFullGrp', [{'NoMDEntries', [                    % group
        [{'MDEntryType', <<"BID">>}]
    ]}]}
]),

% message deserialization
BinaryMessage = <<"8=FIX.4.4", 1, "9=102", 1, "35=A", 1,
    "212=1", 1, "213=", 1, 1,"49=me", 1, "56=you", 1,
    "34=1", 1, "52=20090107-18:15:16", 1, "98=0", 1, "108=60", 1, "384=2", 1,
    "372=abc", 1, "385=S", 1, "372=def", 1, "385=R", 1, "10=232", 1
>>,
{ok, 'Logon', TagsMarkup, <<>>} = erlyfix_parser:parse(BinaryMessage, Protocol),

```

TagsMarkup is flat list of tags uplifted to fields/groups/components. It is expected to be processed by `lists:foldl/3`. Tagsmarkup is something like that:

```erlang
[
    {start, header,{}},
        {field, 'BeginString', F_BeginString, <<"FIX.4.4">>},
        {field, 'BodyLength', F_BodyLength, 102},
        {field, 'MsgType', F_MsgType, <<"A">>},
        {field, 'XmlDataLen', F_XmlDataLen, <<"1">>},
        {field, 'XmlData', F_XmlData, <<1>>},
        {field, 'SenderCompID', F_SenderCompID, <<"me">>},
        {field, 'TargetCompID',F_TargetCompID, <<"you">>},
        {field, 'MsgSeqNum', F_MsgSeqNum, <<"1">>},
        {field, 'SendingTime', F_SendingTime, <<"20090107-18:15:16">>},
    {finish,header},
    {start,body,{}},
        {field, 'EncryptMethod', F_EncryptMethod, <<"0">>},
        {field, 'HeartBtInt', F_HeartBtInt, <<"60">>},
        {start,group,{'NoMsgTypes',2}},
            {field, 'RefMsgType', F_RefMsgType, <<"abc">>},
            {field, 'MsgDirection', F_MsgDirection, << "S">>},
            {field, 'RefMsgType', F_RefMsgType, <<"def">>},
            {field, 'MsgDirection', F_MsgDirection, <<"R">>},
        {finish,group},
    {finish,body},
    {start,trailer,{}},
        {field, 'CheckSum', F_CheckSum, 232},
    {finish,trailer}
],
```

where `F_*` is opaque field structure. Please note, that identations are for humans-only, the list itself is flat

Here is an example of folding tags markup:

```erlang

-record(quote, {
    price,
    volume,
    source
}).
-record(tick, {
    symbol,
    bid,
    ask
}).
...

{ok, IoList} = erlyfix_protocol:serialize(Protocol, 'MarketDataSnapshotFullRefresh', [
    {'SenderCompID', <<"me">>},
    {'TargetCompID', <<"you">>},
    {'MsgSeqNum', 1},
    {'SendingTime', <<"20171109-16:19:07.541">>},
    {'Instrument', [{'Symbol', <<"EURCHF">>}] },
    {'MDReqID', <<"31955:1510225047.01637:EURCHF">>},
    {'MDFullGrp', [{'NoMDEntries', [
        [{'MDEntryType', <<"BID">>}, {'MDEntryPx', <<"1.07509">>},
            {'MDEntrySize', <<"200000">>}, {'QuoteCondition', <<"OPEN">>},
            {'MDEntryOriginator', <<"PromoXM">>, {'QuoteEntryID', <<"82837831">>}}],
        [{'MDEntryType', <<"OFFER">>}, {'MDEntryPx', <<"1.07539">>},
            {'MDEntrySize', <<"100000">>}, {'QuoteCondition', <<"OPEN">>},
            {'MDEntryOriginator', <<"PromoXM1">>, {'QuoteEntryID', <<"82837832">>}}]
    ]}]}
]),
Msg = iolist_to_binary(IoList),
{ok, 'MarketDataSnapshotFullRefresh', Markup, <<>>} = erlyfix_parser:parse(Msg, Protocol),

M2Q = fun(M) ->
    #quote{
        price = maps:get(price, M),
        volume = maps:get(volume, M),
        source = maps:get(source, M)
    }
end,

F = fun(E, {Result, Stack} = Acc ) ->
    case E of
        {field, 'Symbol', _F, V} -> {ok, [ {symbol, V} | Stack ]};
        {field, 'MDEntryType', F, V} ->
            case erlyfix_fields:as_label(V, F) of
                <<"BID">> -> {ok, [{bid, #{} } | Stack]};
                <<"OFFER">> -> {ok, [{ask, #{}} | Stack]}
            end;
        {field, 'MDEntryPx', _F, V} ->
            [{Type, Map0} | T] = Stack,
            Price = binary_to_float(V),
            {ok, [ {Type, Map0#{price => Price} } | T ] };
        {field, 'MDEntrySize', _F, V} ->
            [{Type, Map0} | T] = Stack,
            Volume = binary_to_integer(V),
            {ok, [ {Type, Map0#{volume => Volume} } | T ] };
        {field, 'MDEntryOriginator', _F, V} ->
            [{Type, Map0} | T] = Stack,
            {ok, [ {Type, Map0#{source => V} } | T ] };
        {start,group,{'NoMDEntries',Count}} ->
            case Count of
                2 -> {ok, [{group, 'NoMsgTypes' } | Stack]};
                _ -> {error, Stack}
            end;
        {finish,group} ->
            case Stack of
                [E1, E2, {group, 'NoMsgTypes' } | T] ->
                    {T1, M1} = E1,
                    Q1 = M2Q(M1),
                    {T2, M2} = E2,
                    Q2 = M2Q(M2),
                    case {T1, T2} of
                        {bid, ask} -> {ok, [Q1, Q2 | T]};
                        {ask, bid} -> {ok, [Q2, Q1 | T]}
                    end;
                _ -> Acc
            end;
        {finish,trailer} ->
            case Result of
                ok ->
                    [Bid, Ask, {symbol, Symbol}] = Stack,
                    Tick = #tick{ bid = Bid, ask = Ask, symbol = Symbol },
                    {ok, Tick};
                _ -> Acc
            end;
        _ -> Acc
    end
end,
{ok, Tick} = lists:foldl(F, {ok, []}, Markup),

```

Build
-----

    $ rebar3 compile


License
-----

Apache 2

See also
-----

https://github.com/maxlapshin/fix - FIX-client implementation as OTP-application, includes network layer.

