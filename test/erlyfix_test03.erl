-module(erlyfix_test03).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

load() ->
    Path = "test/FIX44.xml",
    {ok, P} = erlyfix_protocol:load(Path),
    P.

as_list(B) ->
    L = binary_to_list(B),
    {L, byte_size(B)}.

fields_test() ->
    P = load(),

    % String
    {ok, F_Account} = erlyfix_protocol:lookup(P, {field, by_name, 'Account' }),
    ok = erlyfix_fields:validate(as_list(<<"String">>), F_Account),
    ok = erlyfix_fields:validate(as_list(<<"">>), F_Account),
    error = erlyfix_fields:validate(as_list(<<1>>), F_Account),
    "String" = erlyfix_fields:convert("String", F_Account),
    not_found = erlyfix_fields:as_label("String", F_Account),

    % String with mapping
    {ok, F_AdvTransType} = erlyfix_protocol:lookup(P, {field, by_name, 'AdvTransType' }),
    <<"NEW">> = erlyfix_fields:as_label("N", F_AdvTransType),
    not_found = erlyfix_fields:as_label("ZZZ", F_AdvTransType),

    % INT
    {ok, F_AllocStatus} = erlyfix_protocol:lookup(P, {field, by_name, 'AllocStatus' }),
    ok = erlyfix_fields:validate(as_list(<<"5">>), F_AllocStatus),
    ok = erlyfix_fields:validate(as_list(<<"5555">>), F_AllocStatus),
    ok = erlyfix_fields:validate(as_list(<<"-5555">>), F_AllocStatus),
    error = erlyfix_fields:validate(as_list(<<1>>), F_AllocStatus),
    error = erlyfix_fields:validate(as_list(<<"string">>), F_AllocStatus),
    error = erlyfix_fields:validate(as_list(<<"+5555">>), F_AllocStatus),
    <<"ACCEPTED">> = erlyfix_fields:as_label("0", F_AllocStatus),
    not_found = erlyfix_fields:as_label("99", F_AllocStatus),
    5555 = erlyfix_fields:convert("5555", F_AllocStatus),

    % LENGTH
    {ok, F_SecureDataLen} = erlyfix_protocol:lookup(P, {field, by_name, 'SecureDataLen' }),
    ok = erlyfix_fields:validate(as_list(<<"5555">>), F_SecureDataLen),
    error = erlyfix_fields:validate(as_list(<<"-5555">>), F_SecureDataLen),

    % DATA
    {ok, F_SecureData} = erlyfix_protocol:lookup(P, {field, by_name, 'SecureData' }),
    ok = erlyfix_fields:validate(as_list(<<"5555">>), F_SecureData),
    ok = erlyfix_fields:validate(as_list(<<1, "5555", 1>>), F_SecureData),
    "zzz" = erlyfix_fields:convert("zzz", F_SecureData),

    % FLOAT
    {ok, F_ContAmtValue} = erlyfix_protocol:lookup(P, {field, by_name, 'ContAmtValue' }),
    ok = erlyfix_fields:validate(as_list(<<"0">>), F_ContAmtValue),
    ok = erlyfix_fields:validate(as_list(<<"3.14">>), F_ContAmtValue),
    ok = erlyfix_fields:validate(as_list(<<"-5">>), F_ContAmtValue),
    ok = erlyfix_fields:validate(as_list(<<"00023.23">>), F_ContAmtValue),
    ok = erlyfix_fields:validate(as_list(<<"-23.0">>), F_ContAmtValue),
    ok = erlyfix_fields:validate(as_list(<<"23.0">>), F_ContAmtValue),
    error = erlyfix_fields:validate(as_list(<<"22.2.2">>), F_ContAmtValue),
    error = erlyfix_fields:validate(as_list(<<"+1">>), F_ContAmtValue),
    error = erlyfix_fields:validate(as_list(<<"abc">>), F_ContAmtValue),
    error = erlyfix_fields:validate(as_list(<<"">>), F_ContAmtValue),
    error = erlyfix_fields:validate(as_list(<<"22.2.2">>), F_ContAmtValue),
    3.14 = erlyfix_fields:convert("3.14", F_ContAmtValue),
    5 = erlyfix_fields:convert("5", F_ContAmtValue),

    % CHAR
    {ok, F_CommType} = erlyfix_protocol:lookup(P, {field, by_name, 'CommType' }),
    ok = erlyfix_fields:validate(as_list(<<"0">>), F_CommType),
    error = erlyfix_fields:validate(as_list(<<"">>), F_CommType),
    error = erlyfix_fields:validate(as_list(<<"zz">>), F_CommType),

    % CURRENCY
    {ok, F_ContAmtCurr} = erlyfix_protocol:lookup(P, {field, by_name, 'ContAmtCurr' }),
    ok = erlyfix_fields:validate(as_list(<<"USD">>), F_ContAmtCurr),
    error = erlyfix_fields:validate(as_list(<<"USDJPY">>), F_ContAmtCurr),
    error = erlyfix_fields:validate(as_list(<<"">>), F_ContAmtCurr),
    "USD" = erlyfix_fields:convert("USD", F_ContAmtCurr),

    % BOOLEAN
    {ok, F_PossDupFlag} = erlyfix_protocol:lookup(P, {field, by_name, 'PossDupFlag' }),
    ok = erlyfix_fields:validate(as_list(<<"Y">>), F_PossDupFlag),
    ok = erlyfix_fields:validate(as_list(<<"N">>), F_PossDupFlag),
    error = erlyfix_fields:validate(as_list(<<"n">>), F_PossDupFlag),
    true = erlyfix_fields:convert(<<"Y">>, F_PossDupFlag),
    false = erlyfix_fields:convert(<<"N">>, F_PossDupFlag),
    <<"YES">> = erlyfix_fields:as_label("Y", F_PossDupFlag),
    <<"NO">> = erlyfix_fields:as_label("N", F_PossDupFlag),

    % COUNTRY
    {ok, F_Country} = erlyfix_protocol:lookup(P, {field, by_name, 'Country' }),
    ok = erlyfix_fields:validate(as_list(<<"BY">>), F_Country),
    error = erlyfix_fields:validate(as_list(<<"BYN">>), F_Country),
    "BY" = erlyfix_fields:convert("BY", F_Country),

    % UTCTIMEONLY
    {ok, F_MDEntryTime} = erlyfix_protocol:lookup(P, {field, by_name, 'MDEntryTime' }),
    ok = erlyfix_fields:validate(as_list(<<"11:12:55">>), F_MDEntryTime),
    ok = erlyfix_fields:validate(as_list(<<"11:12:55.123">>), F_MDEntryTime),
    error = erlyfix_fields:validate(as_list(<<"25:12:55">>), F_MDEntryTime),
    error = erlyfix_fields:validate(as_list(<<"11:62:55">>), F_MDEntryTime),
    error = erlyfix_fields:validate(as_list(<<"11:12:65">>), F_MDEntryTime),
    error = erlyfix_fields:validate(as_list(<<"11:12:55.1244">>), F_MDEntryTime),
    ?assertEqual(
        #utc_time { hour = 11, minute = 12, second = 55, ms = 0},
        erlyfix_fields:convert("11:12:55", F_MDEntryTime)
    ),
    ?assertEqual(
        #utc_time { hour = 11, minute = 12, second = 55, ms = 123},
        erlyfix_fields:convert("11:12:55.123", F_MDEntryTime)
    ),

    % MONTHYEAR
    {ok, F_MaturityMonthYear} = erlyfix_protocol:lookup(P, {field, by_name, 'MaturityMonthYear' }),
    ok = erlyfix_fields:validate(as_list(<<"199812">>), F_MaturityMonthYear),
    ok = erlyfix_fields:validate(as_list(<<"19981210">>), F_MaturityMonthYear),
    ok = erlyfix_fields:validate(as_list(<<"199812w1">>), F_MaturityMonthYear),
    ok = erlyfix_fields:validate(as_list(<<"199812w5">>), F_MaturityMonthYear),
    error = erlyfix_fields:validate(as_list(<<"19981232">>), F_MaturityMonthYear),
    error = erlyfix_fields:validate(as_list(<<"199812w7">>), F_MaturityMonthYear),
    error = erlyfix_fields:validate(as_list(<<"garbage">>), F_MaturityMonthYear),
    ?assertEqual(
        #monthyear_week{ year = 1998, month = 12, week = 0 },
        erlyfix_fields:convert("199812", F_MaturityMonthYear)
    ),
    ?assertEqual(
        #monthyear_week{ year = 1998, month = 12, week = 1 },
        erlyfix_fields:convert("199812w1", F_MaturityMonthYear)
    ),
    ?assertEqual(
        #monthyear_day{ year = 1998, month = 12, day = 10 },
        erlyfix_fields:convert("19981210", F_MaturityMonthYear)
    ),

    % LOCALMKTDATE
    {ok, F_IssueDate} = erlyfix_protocol:lookup(P, {field, by_name, 'IssueDate' }),
    ok = erlyfix_fields:validate(as_list(<<"19981231">>), F_IssueDate),
    error = erlyfix_fields:validate(as_list(<<"199812">>), F_IssueDate),
    error = erlyfix_fields:validate(as_list(<<"19981231111">>), F_IssueDate),
    ?assertEqual(
        #fix_date { year = 1998, month = 12, day = 31},
        erlyfix_fields:convert("19981231", F_IssueDate)
    ),

    % UTCTIMESTAMP
    {ok, F_LastUpdateTime} = erlyfix_protocol:lookup(P, {field, by_name, 'LastUpdateTime' }),
    ok = erlyfix_fields:validate(as_list(<<"19981231-23:59:59">>), F_LastUpdateTime),
    ok = erlyfix_fields:validate(as_list(<<"19981231-23:59:59.123">>), F_LastUpdateTime),
    error = erlyfix_fields:validate(as_list(<<"1998123123:59:59.123">>), F_LastUpdateTime),
    error = erlyfix_fields:validate(as_list(<<"1998123123:59:59.1231">>), F_LastUpdateTime),
    error = erlyfix_fields:validate(as_list(<<"garbage">>), F_LastUpdateTime),
    ?assertEqual(
        #utc_timestamp {
            date = #fix_date { year = 1998, month = 12, day = 31 },
            time = #utc_time { hour = 23, minute = 59, second = 59, ms = 123}
        },
        erlyfix_fields:convert("19981231-23:59:59.123", F_LastUpdateTime)
    ),

    % check for aliases

    % MULTIPLEVALUESTRING
    {ok, F_ExecInst} = erlyfix_protocol:lookup(P, {field, by_name, 'ExecInst' }),
    ok = erlyfix_fields:validate(as_list(<<"String">>), F_ExecInst),
    "String" = erlyfix_fields:convert("String", F_ExecInst),
    % EXCHANGE
    {ok, F_LastMkt} = erlyfix_protocol:lookup(P, {field, by_name, 'LastMkt' }),
    ok = erlyfix_fields:validate(as_list(<<"String">>), F_LastMkt),
    "String" = erlyfix_fields:convert("String", F_LastMkt),
    % SEQNUM
    {ok, F_MsgSeqNum} = erlyfix_protocol:lookup(P, {field, by_name, 'MsgSeqNum' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_MsgSeqNum),
    123 = erlyfix_fields:convert("123", F_MsgSeqNum),
    % NUMINGROUP
    {ok, F_NoExecs} = erlyfix_protocol:lookup(P, {field, by_name, 'NoExecs' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_NoExecs),
    123 = erlyfix_fields:convert("123", F_NoExecs),
    % ATM
    {ok, F_Concession} = erlyfix_protocol:lookup(P, {field, by_name, 'Concession' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_Concession),
    123 = erlyfix_fields:convert("123", F_Concession),
    % PERCENTAGE
    {ok, F_LegRepurchaseRate} = erlyfix_protocol:lookup(P, {field, by_name, 'LegRepurchaseRate' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_LegRepurchaseRate),
    123 = erlyfix_fields:convert("123", F_LegRepurchaseRate),
    % PRICE
    {ok, F_BasisFeaturePrice} = erlyfix_protocol:lookup(P, {field, by_name, 'BasisFeaturePrice' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_BasisFeaturePrice),
    123 = erlyfix_fields:convert("123", F_BasisFeaturePrice),
    % QTY
    {ok, F_DefBidSize} = erlyfix_protocol:lookup(P, {field, by_name, 'DefBidSize' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_DefBidSize),
    123 = erlyfix_fields:convert("123", F_DefBidSize),
    % PRICEOFFSET
    {ok, F_PriceImprovement} = erlyfix_protocol:lookup(P, {field, by_name, 'PriceImprovement' }),
    ok = erlyfix_fields:validate(as_list(<<"123">>), F_PriceImprovement),
    123 = erlyfix_fields:convert("123", F_PriceImprovement),
    % UTCDATEONLY
    {ok, F_MDEntryDate} = erlyfix_protocol:lookup(P, {field, by_name, 'MDEntryDate' }),
    ok = erlyfix_fields:validate(as_list(<<"19981231">>), F_MDEntryDate),
    ?assertEqual(
        #fix_date { year = 1998, month = 12, day = 31},
        erlyfix_fields:convert("19981231", F_MDEntryDate)
    ).

