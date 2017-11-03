-module(erlyfix_test04).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).

fields_test() ->
    P = load(),

    % String
    {ok, F_Account} = erlyfix_protocol:lookup(P, {field, by_name, 'Account' }),
    ok = erlyfix_fields:validate(<<"String">>, F_Account),
    ok = erlyfix_fields:validate(<<"">>, F_Account),
    error = erlyfix_fields:validate(<<1>>, F_Account),
    <<"String">> = erlyfix_fields:convert(<<"String">>, F_Account),
    not_found = erlyfix_fields:as_label(<<"String">>, F_Account),

    % String with mapping
    {ok, F_AdvTransType} = erlyfix_protocol:lookup(P, {field, by_name, 'AdvTransType' }),
    <<"NEW">> = erlyfix_fields:as_label(<<"N">>, F_AdvTransType),
    not_found = erlyfix_fields:as_label(<<"ZZZ">>, F_AdvTransType),

    % INT
    {ok, F_AllocStatus} = erlyfix_protocol:lookup(P, {field, by_name, 'AllocStatus' }),
    ok = erlyfix_fields:validate(<<"5">>, F_AllocStatus),
    ok = erlyfix_fields:validate(<<"5555">>, F_AllocStatus),
    ok = erlyfix_fields:validate(<<"-5555">>, F_AllocStatus),
    error = erlyfix_fields:validate(<<1>>, F_AllocStatus),
    error = erlyfix_fields:validate(<<"string">>, F_AllocStatus),
    error = erlyfix_fields:validate(<<"+5555">>, F_AllocStatus),
    <<"ACCEPTED">> = erlyfix_fields:as_label(<<"0">>, F_AllocStatus),
    not_found = erlyfix_fields:as_label(<<"99">>, F_AllocStatus),

    % LENGTH
    {ok, F_SecureDataLen} = erlyfix_protocol:lookup(P, {field, by_name, 'SecureDataLen' }),
    ok = erlyfix_fields:validate(<<"5555">>, F_SecureDataLen),
    error = erlyfix_fields:validate(<<"-5555">>, F_SecureDataLen),

    % DATA
    {ok, F_SecureData} = erlyfix_protocol:lookup(P, {field, by_name, 'SecureData' }),
    ok = erlyfix_fields:validate(<<"5555">>, F_SecureData),
    ok = erlyfix_fields:validate(<<1, "5555", 1>>, F_SecureData),

    % FLOAT
    {ok, F_ContAmtValue} = erlyfix_protocol:lookup(P, {field, by_name, 'ContAmtValue' }),
    ok = erlyfix_fields:validate(<<"0">>, F_ContAmtValue),
    ok = erlyfix_fields:validate(<<"3.14">>, F_ContAmtValue),
    ok = erlyfix_fields:validate(<<"-5">>, F_ContAmtValue),
    ok = erlyfix_fields:validate(<<"00023.23">>, F_ContAmtValue),
    ok = erlyfix_fields:validate(<<"-23.0">>, F_ContAmtValue),
    ok = erlyfix_fields:validate(<<"23.0">>, F_ContAmtValue),
    error = erlyfix_fields:validate(<<"22.2.2">>, F_ContAmtValue),
    error = erlyfix_fields:validate(<<"+1">>, F_ContAmtValue),
    error = erlyfix_fields:validate(<<"abc">>, F_ContAmtValue),
    error = erlyfix_fields:validate(<<"">>, F_ContAmtValue),
    error = erlyfix_fields:validate(<<"22.2.2">>, F_ContAmtValue),
    3.14 = erlyfix_fields:convert(<<"3.14">>, F_ContAmtValue),

    % CHAR
    {ok, F_CommType} = erlyfix_protocol:lookup(P, {field, by_name, 'CommType' }),
    ok = erlyfix_fields:validate(<<"0">>, F_CommType),
    error = erlyfix_fields:validate(<<"">>, F_CommType),
    error = erlyfix_fields:validate(<<"zz">>, F_CommType).

