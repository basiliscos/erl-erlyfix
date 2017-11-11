-module(erlyfix_test02).
-include_lib("eunit/include/eunit.hrl").
-include("include/erlyfix.hrl").

load() ->
    Path = "test/FIX44.xml",
    {ok, P} = erlyfix_protocol:load(Path),
    P.

serialization_Logon_Missing_Header_Field_test() ->
    P = load(),
    {error, Reason} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"From-me">>},
        {'TargetCompID', <<"To-you">>},
        {'SendingTime', '20090107-18:15:16'},
        {'EncryptMethod', <<"NONE">>},
        {'Username', <<"Login">>},
        {'Password', <<"Pass">>},
        {'HeartBtInt', 20}
    ]),
    ?assertEqual(<<"Missing mandatory 'MsgSeqNum' for 'Logon'">>, Reason).

serialization_Logon_Missing_Mandatory_Field_test() ->
    P = load(),
    {error, Reason} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"From-me">>},
        {'TargetCompID', <<"To-you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', '20090107-18:15:16'},
        {'Username', <<"Login">>},
        {'Password', <<"Pass">>},
        {'HeartBtInt', 20}
    ]),
    ?assertEqual(<<"Missing mandatory 'EncryptMethod' for 'Logon'">>, Reason).

serialization_Wrong_Value_For_Field_test() ->
    P = load(),
    {error, Reason} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"From-me">>},
        {'TargetCompID', <<"To-you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', '20090107-18:15:16'},
        {'EncryptMethod', <<"WRONG">>},
        {'Username', <<"Login">>},
        {'Password', <<"Pass">>},
        {'HeartBtInt', 20}
    ]),
    ?assertEqual(<<"Description 'WRONG' is not available for field 'EncryptMethod'">>, Reason).

serialization_Wrong_message_test() ->
    P = load(),
    {error, Reason} = erlyfix_protocol:serialize(P, 'zzz', []),
    ?assertEqual(<<"Message 'zzz' not found">>, Reason).

serialization_wrong_component_test() ->
    P = load(),
    {error, Reason} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', <<"20090107-18:15:16">>},
        {'EncryptMethod', <<"NONE">>},
        {'HeartBtInt', 60},
        {'Instrument', [{'Symbol', 'USDJPY'}] }
    ]),
    ?assertEqual(<<"'Instrument' is not available for 'Logon'">>, Reason).

serialization_group_with_wrong_field_test() ->
    P = load(),
    {error, Reason} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', <<"20090107-18:15:16">>},
        {'EncryptMethod', <<"NONE">>},
        {'HeartBtInt', 60},
        {'NoMsgTypes', [
            [{'HeartBtInt', 60}]
        ]}
    ]),
    ?assertEqual(<<"'HeartBtInt' is not available for 'NoMsgTypes'">>, Reason).


serialization_Logon_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', <<"20090107-18:15:16">>},
        {'EncryptMethod', <<"NONE">>},
        {'HeartBtInt', 60}
    ]),
    M = iolist_to_binary(IoList),
    M_expected = <<"8=FIX.4.4", 1, "9=56", 1, "35=A", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "98=0", 1, "108=60", 1, "10=110", 1
    >>,
    ?assertEqual(M_expected, M).

serialization_Logon_with_group_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', '20090107-18:15:16'},
        {'EncryptMethod', <<"NONE">>},
        {'HeartBtInt', 60},
        {'NoMsgTypes', [
            [{'RefMsgType', <<"abc">>}, {'MsgDirection', <<"SEND">>}],
            [{'RefMsgType', <<"def">>}, {'MsgDirection', <<"RECEIVE">>}]
        ]}
    ]),
    M = iolist_to_binary(IoList),
    M_expected = <<"8=FIX.4.4", 1, "9=90", 1, "35=A", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "98=0", 1, "108=60", 1, "384=2", 1,
        "372=abc", 1, "385=S", 1, "372=def", 1, "385=R", 1, "10=229", 1
    >>,
    ?assertEqual(M_expected, M).

serialization_empty_group_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', '20090107-18:15:16'},
        {'EncryptMethod', <<"NONE">>},
        {'HeartBtInt', 60},
        {'NoMsgTypes', []}
    ]),
    M = iolist_to_binary(IoList),
    M_expected = <<"8=FIX.4.4", 1, "9=56", 1, "35=A", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "98=0", 1, "108=60", 1, "10=110", 1
    >>,
    ?assertEqual(M_expected, M).

serialization_Advertisement_with_component_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'Advertisement', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', '20090107-18:15:16'},
        {'AdvId', <<"some-id">>},
        {'AdvTransType', <<"NEW">>},
        {'AdvSide', <<"BUY">>},
        {'Quantity', 5},
        {'Instrument', [{'Symbol', 'USDJPY'}] }
    ]),
    M = iolist_to_binary(IoList),
    M_expected = <<"8=FIX.4.4", 1, "9=77", 1, "35=7", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "2=some-id", 1, "5=N", 1,
        "4=B", 1, "53=5", 1, "55=USDJPY", 1, "10=064", 1
    >>,
    ?assertEqual(M_expected, M).

serialization_MarketDataSnapshotFullRefresh_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'MarketDataSnapshotFullRefresh', [
        {'SenderCompID', <<"me">>},
        {'TargetCompID', <<"you">>},
        {'MsgSeqNum', 1},
        {'SendingTime', <<"20171109-16:19:07.541">>},
        {'Instrument', [{'Symbol', <<"EURCHF">>}] },
        {'MDReqID', <<"31955:1510225047.01637:EURCHF">>},
        {'MDFullGrp', [{'NoMDEntries', [
            [{'MDEntryType', <<"BID">>}]
        ]}]}
    ]),
    M = iolist_to_binary(IoList),
    M_expected = <<"8=FIX.4.4", 1, "9=104", 1, "35=W", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20171109-16:19:07.541", 1, "55=EURCHF", 1,
        "262=31955:1510225047.01637:EURCHF", 1, "268=1", 1, "269=0", 1, "10=027", 1
    >>,
    %?debugFmt("Message(r) == ~s", [M]),
    %?debugFmt("Message(e) == ~s", [M_expected]),
    %?debugFmt("Message(r2) == ~w", [M]),
    %?debugFmt("Message(e2) == ~w", [M_expected]),
    ?assertEqual(M_expected, M).
