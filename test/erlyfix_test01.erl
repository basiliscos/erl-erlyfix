-module(erlyfix_test01).
-include_lib("eunit/include/eunit.hrl").

load() ->
    %code:lib_dir(Name, priv)
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).


protocol_load_test() ->
    Proto = load().
