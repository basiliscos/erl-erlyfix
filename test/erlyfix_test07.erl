-module(erlyfix_test07).
-include_lib("eunit/include/eunit.hrl").
-include("include/erlyfix.hrl").

-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

load() ->
    Path = "test/FIX44.xml",
    {ok, P} = erlyfix_protocol:load(Path),
    P.

message_with_component_parse_test() ->
    P = load(),
    M = <<"8=FIX.4.4", 1, "9=66", 1, "35=Y", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "262=abc", 1, "816=1", 1,
        "817=def", 1, "10=132", 1
    >>,
    {ok, 'MarketDataRequestReject', TagsMarkup, <<>>} = erlyfix_parser:parse(M, P),
    GetField = fun(Name) ->
        {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, Name}),
        F
    end,
    Markup_Expected = [
        {start, header,{}},
            {field, 'BeginString', GetField('BeginString'), <<"FIX.4.4">>},
            {field, 'BodyLength', GetField('BodyLength'), 66},
            {field, 'MsgType', GetField('MsgType'), <<"Y">>},
            {field, 'SenderCompID', GetField('SenderCompID'), <<"me">>},
            {field, 'TargetCompID', GetField('TargetCompID'), <<"you">>},
            {field, 'MsgSeqNum', GetField('MsgSeqNum'), <<"1">>},
            {field, 'SendingTime', GetField('SendingTime'), <<"20090107-18:15:16">>},
        {finish,header},
        {start,body,{}},
            {field, 'MDReqID', GetField('MDReqID'), <<"abc">>},
            {start,component,{'MDRjctGrp'}},
                {start,group,{'NoAltMDSource',1}},
                    {field, 'AltMDSourceID', GetField('AltMDSourceID'), <<"def">>},
                {finish,group},
            {finish,component},
        {finish,body},
        {start,trailer,{}},
            {field, 'CheckSum', GetField('CheckSum'), 132},
        {finish,trailer}
    ],
    % ?DEBUG(Markup_Expected),
    % ?DEBUG(TagsMarkup),
    ?assertEqual(Markup_Expected, TagsMarkup).

complex_message_parse_test() ->
    P = load(),
    M = <<"8=FIX.4.4", 1, "9=108", 1, "35=6", 1, "49=me", 1, "56=you", 1, "34=1", 1, "52=20090107-18:15:16", 1,
        "23=abc", 1, "28=C", 1, "27=L", 1, "54=G", 1, "55=EURUSD", 1, "864=3", 1, "865=1", 1,
        "865=2", 1, "865=99", 1, "38=499", 1, "10=100", 1
    >>,
    {ok, 'IOI', TagsMarkup, <<>>} = erlyfix_parser:parse(M, P),
    GetField = fun(Name) ->
        {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, Name}),
        F
    end,
    Markup_Expected = [
        {start, header,{}},
            {field, 'BeginString', GetField('BeginString'), <<"FIX.4.4">>},
            {field, 'BodyLength', GetField('BodyLength'), 108},
            {field, 'MsgType', GetField('MsgType'), <<"6">>},
            {field, 'SenderCompID', GetField('SenderCompID'), <<"me">>},
            {field, 'TargetCompID', GetField('TargetCompID'), <<"you">>},
            {field, 'MsgSeqNum', GetField('MsgSeqNum'), <<"1">>},
            {field, 'SendingTime', GetField('SendingTime'), <<"20090107-18:15:16">>},
        {finish,header},
        {start,body,{}},
            {field, 'IOIID', GetField('IOIID'), <<"abc">>},
            {field, 'IOITransType', GetField('IOITransType'), <<"C">>},
            {field, 'IOIQty', GetField('IOIQty'), <<"L">>},
            {field, 'Side', GetField('Side'), <<"G">>},
            {start,component,{'Instrument'}},
                {field, 'Symbol', GetField('Symbol'), <<"EURUSD">>},
                {start,component,{'EvntGrp'}},
                    {start,group,{'NoEvents',3}},
                        {field, 'EventType', GetField('EventType'), <<"1">>},
                        {field, 'EventType', GetField('EventType'), <<"2">>},
                        {field, 'EventType', GetField('EventType'), <<"99">>},
                    {finish,group},
                {finish,component},
            {finish,component},
            {start,component,{'OrderQtyData'}},
                {field, 'OrderQty', GetField('OrderQty'), <<"499">>},
            {finish,component},
        {finish,body},
        {start,trailer,{}},
            {field, 'CheckSum', GetField('CheckSum'), 100},
        {finish,trailer}
    ],
    ?DEBUG(TagsMarkup),
    ?assertEqual(Markup_Expected, TagsMarkup).
