-module(erlyfix_test02).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).

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
    %?debugFmt("Message(r) == ~s", [M]),
    %?debugFmt("Message(e) == ~s", [M_expected]),
    %?debugFmt("Message(r2) == ~w", [M]),
    %?debugFmt("Message(e2) == ~w", [M_expected]),
    ?assertEqual(M_expected, M).

