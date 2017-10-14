-module(erlyfix_test02).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).

serialization_Logon_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', "From-me"},
        {'TargetCompID', "To-you"},
        {'MsgSeqNum', 1},
        {'SendingTime', '20090107-18:15:16'},

        {'EncryptMethod', 'NONE'},
        {'Username', "Login"},
        {'Password', "Pass"},
        {'HeartBtInt', 20}
    ]),
    M = iolist_to_binary(IoList),
    M_expected = <<"8=FIX.4.4", 1, "9=86", 1, "35=A", 1, "49=From-me", 1, "56=To-you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "98=NONE", 1, "553=Login", 1,
        "554=Pass", 1, "108=20", 1, "10=196", 1
    >>,
    %?debugFmt("Message(r) == ~s, ~w", [M, M]),
    %?debugFmt("Message(e) == ~s, ~w", [M_expected, M_expected]),
    ?assertEqual(M_expected, M).

