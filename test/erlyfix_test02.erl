-module(erlyfix_test02).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).

protocol_load_test() ->
    P = load(),
    {ok, IoList} = erlyfix_protocol:serialize(P, 'Logon', [
        {'SenderCompID', "From-me"},
        {'TargetCompID', "To-you"},
        {'Username', "Login"},
        {'Password', "Pass"},
        {'HeartBtInt', 20}
    ]).
