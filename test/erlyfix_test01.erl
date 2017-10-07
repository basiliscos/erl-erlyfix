-module(erlyfix_test01).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).


protocol_load_test() ->
    P = load(),
    ?assertEqual(#protocol_version{ major = 4, minor = 4, servicepack = 0 },
        erlyfix_protocol:version(P)),

    % simple field
    {ok, F_account_1} = erlyfix_protocol:lookup(P, {field, by_name, "Account" }),
    ?assertEqual("Account", F_account_1#field.name),
    ?assertEqual("1", F_account_1#field.number),
    ?assertEqual(#{}, F_account_1#field.value4key),
    ?assertEqual(#{}, F_account_1#field.value4description),
    {ok, F_account_2} = erlyfix_protocol:lookup(P, {field, by_number, "1" }),
    ?assertEqual(F_account_1, F_account_2),

    % field with values
    {ok, F_AdvSide} = erlyfix_protocol:lookup(P, {field, by_name, "AdvSide" }),
    V_BUY_1 = maps:get("BUY", F_AdvSide#field.value4description),
    ?assertEqual("B", V_BUY_1#value_def.key),
    ?assertEqual("BUY", V_BUY_1#value_def.description),
    V_BUY_2 = maps:get("B", F_AdvSide#field.value4key),
    ?assertEqual(V_BUY_1, V_BUY_2),
    %?debugFmt("value4description == ~p", [F_AdvSide#field.value4description]),
    {ok, _V_SELL} = maps:find("SELL", F_AdvSide#field.value4description),
    {ok, _V_CROSS} = maps:find("CROSS", F_AdvSide#field.value4description),
    {ok, _V_TRADE} = maps:find("TRADE", F_AdvSide#field.value4description).


