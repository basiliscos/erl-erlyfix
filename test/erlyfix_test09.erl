-module(erlyfix_test09).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix.hrl").
-include("debug.hrl").

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


load() ->
    {ok, P} = erlyfix_protocol:load("test/FIX44.xml"),
    P.

sample_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'MarketDataSnapshotFullRefresh', [
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
    {ok, 'MarketDataSnapshotFullRefresh', Markup, <<>>} = erlyfix_parser:parse(Msg, P),

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
            {start,group,{'NoMDEntries', 2}} -> Acc;
            {start,group,{'NoMDEntries', _}} -> {error, Stack};
            {finish,group,{'NoMDEntries', 2}} ->
                [E1, E2 | T] = Stack,
                {T1, M1} = E1,
                Q1 = M2Q(M1),
                {T2, M2} = E2,
                Q2 = M2Q(M2),
                case {T1, T2} of
                    {bid, ask} -> {ok, [Q1, Q2 | T]};
                    {ask, bid} -> {ok, [Q2, Q1 | T]}
                end;
            {finish,trailer,_} ->
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
    % ?DEBUG(Tick),
    ?assertEqual(#tick{
        symbol = <<"EURCHF">>,
        bid = #quote{ price = 1.07509, volume = 200000, source = <<"PromoXM">> },
        ask = #quote{ price = 1.07539, volume = 100000, source = <<"PromoXM1">> }
    }, Tick).

