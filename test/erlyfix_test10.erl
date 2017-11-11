-module(erlyfix_test10).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

load_test() ->
    MainPath = "priv/protocols/FIX44.xml",
    Extension = "test/extension-sample.xml",
    {ok, P} = erlyfix_protocol:load(MainPath, Extension),
    {ok, F_AwesomeField} = erlyfix_protocol:lookup(P, {field, by_name, 'AwesomeField'}),
    ?assertEqual('AwesomeField', F_AwesomeField#field.name),
    ?assertEqual(33000, F_AwesomeField#field.number),
    ?assertEqual(#{}, F_AwesomeField#field.value4key),
    ?assertEqual(#{}, F_AwesomeField#field.value4description),

    {ok, M_Logon} = erlyfix_protocol:lookup(P, {message, by_name, 'Logon' }),
    {ok, F_AwesomeField} = maps:find('AwesomeField', M_Logon#message.composite4name),
    {ok, F_AwesomeField} = maps:find('AwesomeField', M_Logon#message.mandatoryComposites).

