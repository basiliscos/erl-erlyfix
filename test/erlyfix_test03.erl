-module(erlyfix_test03).
-include_lib("eunit/include/eunit.hrl").
-include("include/erlyfix.hrl").
-include("debug.hrl").

load() ->
    Path = "test/FIX44.xml",
    {ok, P} = erlyfix_protocol:load(Path),
    P.

fields_test() ->
    P = load(),
    H = P#protocol.parser_helpers#parser_helpers.field_REs,
    % String
    {ok, F_Account} = erlyfix_utils:lookup(P, {field, by_name, 'Account' }),
    ok = erlyfix_fields:validate(<<"String">>, F_Account, H),
    ok = erlyfix_fields:validate(<<"">>, F_Account, H),
    error = erlyfix_fields:validate(<<1>>, F_Account, H),
    <<"String">> = erlyfix_fields:convert(<<"String">>, F_Account),
    not_found = erlyfix_fields:as_label(<<"String">>, F_Account),

    % String with mapping
    {ok, F_AdvTransType} = erlyfix_utils:lookup(P, {field, by_name, 'AdvTransType' }),
    <<"NEW">> = erlyfix_fields:as_label(<<"N">>, F_AdvTransType),
    not_found = erlyfix_fields:as_label(<<"ZZZ">>, F_AdvTransType),

    % INT
    {ok, F_AllocStatus} = erlyfix_utils:lookup(P, {field, by_name, 'AllocStatus' }),
    ok = erlyfix_fields:validate(<<"5">>, F_AllocStatus, H),
    ok = erlyfix_fields:validate(<<"5555">>, F_AllocStatus, H),
    ok = erlyfix_fields:validate(<<"-5555">>, F_AllocStatus, H),
    error = erlyfix_fields:validate(<<1>>, F_AllocStatus, H),
    error = erlyfix_fields:validate(<<"string">>, F_AllocStatus, H),
    error = erlyfix_fields:validate(<<"+5555">>, F_AllocStatus, H),
    <<"ACCEPTED">> = erlyfix_fields:as_label(<<"0">>, F_AllocStatus),
    not_found = erlyfix_fields:as_label(<<"99">>, F_AllocStatus),
    5555 = erlyfix_fields:convert(<<"5555">>, F_AllocStatus),

    % LENGTH
    {ok, F_SecureDataLen} = erlyfix_utils:lookup(P, {field, by_name, 'SecureDataLen' }),
    ok = erlyfix_fields:validate(<<"5555">>, F_SecureDataLen, H),
    error = erlyfix_fields:validate(<<"-5555">>, F_SecureDataLen, H),

    % DATA
    {ok, F_SecureData} = erlyfix_utils:lookup(P, {field, by_name, 'SecureData' }),
    ok = erlyfix_fields:validate(<<"5555">>, F_SecureData, H),
    ok = erlyfix_fields:validate(<<1, "5555", 1>>, F_SecureData, H),
    <<"zzz">> = erlyfix_fields:convert(<<"zzz">>, F_SecureData),

    % FLOAT
    {ok, F_ContAmtValue} = erlyfix_utils:lookup(P, {field, by_name, 'ContAmtValue' }),
    ok = erlyfix_fields:validate(<<"0">>, F_ContAmtValue, H),
    ok = erlyfix_fields:validate(<<"3.14">>, F_ContAmtValue, H),
    ok = erlyfix_fields:validate(<<"-5">>, F_ContAmtValue, H),
    ok = erlyfix_fields:validate(<<"00023.23">>, F_ContAmtValue, H),
    ok = erlyfix_fields:validate(<<"-23.0">>, F_ContAmtValue, H),
    ok = erlyfix_fields:validate(<<"23.0">>, F_ContAmtValue, H),
    error = erlyfix_fields:validate(<<"22.2.2">>, F_ContAmtValue, H),
    error = erlyfix_fields:validate(<<"+1">>, F_ContAmtValue, H),
    error = erlyfix_fields:validate(<<"abc">>, F_ContAmtValue, H),
    error = erlyfix_fields:validate(<<"">>, F_ContAmtValue, H),
    error = erlyfix_fields:validate(<<"22.2.2">>, F_ContAmtValue, H),
    3.14 = erlyfix_fields:convert(<<"3.14">>, F_ContAmtValue),
    5 = erlyfix_fields:convert(<<"5">>, F_ContAmtValue),

    % CHAR
    {ok, F_CommType} = erlyfix_utils:lookup(P, {field, by_name, 'CommType' }),
    ok = erlyfix_fields:validate(<<"0">>, F_CommType, H),
    error = erlyfix_fields:validate(<<"">>, F_CommType, H),
    error = erlyfix_fields:validate(<<"zz">>, F_CommType, H),

    % CURRENCY
    {ok, F_ContAmtCurr} = erlyfix_utils:lookup(P, {field, by_name, 'ContAmtCurr' }),
    ok = erlyfix_fields:validate(<<"USD">>, F_ContAmtCurr, H),
    error = erlyfix_fields:validate(<<"USDJPY">>, F_ContAmtCurr, H),
    error = erlyfix_fields:validate(<<"">>, F_ContAmtCurr, H),
    <<"USD">> = erlyfix_fields:convert(<<"USD">>, F_ContAmtCurr),

    % BOOLEAN
    {ok, F_PossDupFlag} = erlyfix_utils:lookup(P, {field, by_name, 'PossDupFlag' }),
    ok = erlyfix_fields:validate(<<"Y">>, F_PossDupFlag, H),
    ok = erlyfix_fields:validate(<<"N">>, F_PossDupFlag, H),
    error = erlyfix_fields:validate(<<"n">>, F_PossDupFlag, H),
    true = erlyfix_fields:convert(<<"Y">>, F_PossDupFlag),
    false = erlyfix_fields:convert(<<"N">>, F_PossDupFlag),
    <<"YES">> = erlyfix_fields:as_label(<<"Y">>, F_PossDupFlag),
    <<"NO">> = erlyfix_fields:as_label(<<"N">>, F_PossDupFlag),

    % COUNTRY
    {ok, F_Country} = erlyfix_utils:lookup(P, {field, by_name, 'Country' }),
    ok = erlyfix_fields:validate(<<"BY">>, F_Country, H),
    error = erlyfix_fields:validate(<<"BYN">>, F_Country, H),
    <<"BY">> = erlyfix_fields:convert(<<"BY">>, F_Country),

    % UTCTIMEONLY
    {ok, F_MDEntryTime} = erlyfix_utils:lookup(P, {field, by_name, 'MDEntryTime' }),
    ok = erlyfix_fields:validate(<<"11:12:55">>, F_MDEntryTime, H),
    ok = erlyfix_fields:validate(<<"11:12:55.123">>, F_MDEntryTime, H),
    error = erlyfix_fields:validate(<<"25:12:55">>, F_MDEntryTime, H),
    error = erlyfix_fields:validate(<<"11:62:55">>, F_MDEntryTime, H),
    error = erlyfix_fields:validate(<<"11:12:65">>, F_MDEntryTime, H),
    error = erlyfix_fields:validate(<<"11:12:55.1244">>, F_MDEntryTime, H),
    ?assertEqual(
        #utc_time { hour = 11, minute = 12, second = 55, ms = 0},
        erlyfix_fields:convert(<<"11:12:55">>, F_MDEntryTime)
    ),
    ?assertEqual(
        #utc_time { hour = 11, minute = 12, second = 55, ms = 123},
        erlyfix_fields:convert(<<"11:12:55.123">>, F_MDEntryTime)
    ),

    % MONTHYEAR
    {ok, F_MaturityMonthYear} = erlyfix_utils:lookup(P, {field, by_name, 'MaturityMonthYear' }),
    ok = erlyfix_fields:validate(<<"199812">>, F_MaturityMonthYear, H),
    ok = erlyfix_fields:validate(<<"19981210">>, F_MaturityMonthYear, H),
    ok = erlyfix_fields:validate(<<"199812w1">>, F_MaturityMonthYear, H),
    ok = erlyfix_fields:validate(<<"199812w5">>, F_MaturityMonthYear, H),
    error = erlyfix_fields:validate(<<"19981232">>, F_MaturityMonthYear, H),
    error = erlyfix_fields:validate(<<"199812w7">>, F_MaturityMonthYear, H),
    error = erlyfix_fields:validate(<<"garbage">>, F_MaturityMonthYear, H),
    ?assertEqual(
        #monthyear_week{ year = 1998, month = 12, week = 0 },
        erlyfix_fields:convert(<<"199812">>, F_MaturityMonthYear)
    ),
    ?assertEqual(
        #monthyear_week{ year = 1998, month = 12, week = 1 },
        erlyfix_fields:convert(<<"199812w1">>, F_MaturityMonthYear)
    ),
    ?assertEqual(
        #monthyear_day{ year = 1998, month = 12, day = 10 },
        erlyfix_fields:convert(<<"19981210">>, F_MaturityMonthYear)
    ),

    % LOCALMKTDATE
    {ok, F_IssueDate} = erlyfix_utils:lookup(P, {field, by_name, 'IssueDate' }),
    ok = erlyfix_fields:validate(<<"19981231">>, F_IssueDate, H),
    error = erlyfix_fields:validate(<<"199812">>, F_IssueDate, H),
    error = erlyfix_fields:validate(<<"19981231111">>, F_IssueDate, H),
    ?assertEqual(
        #fix_date { year = 1998, month = 12, day = 31},
        erlyfix_fields:convert(<<"19981231">>, F_IssueDate)
    ),

    % UTCTIMESTAMP
    {ok, F_LastUpdateTime} = erlyfix_utils:lookup(P, {field, by_name, 'LastUpdateTime' }),
    ok = erlyfix_fields:validate(<<"19981231-23:59:59">>, F_LastUpdateTime, H),
    ok = erlyfix_fields:validate(<<"19981231-23:59:59.123">>, F_LastUpdateTime, H),
    error = erlyfix_fields:validate(<<"1998123123:59:59.123">>, F_LastUpdateTime, H),
    error = erlyfix_fields:validate(<<"1998123123:59:59.1231">>, F_LastUpdateTime, H),
    error = erlyfix_fields:validate(<<"garbage">>, F_LastUpdateTime, H),
    ?assertEqual(
        #utc_timestamp {
            date = #fix_date { year = 1998, month = 12, day = 31 },
            time = #utc_time { hour = 23, minute = 59, second = 59, ms = 123}
        },
        erlyfix_fields:convert(<<"19981231-23:59:59.123">>, F_LastUpdateTime)
    ),

    % check for aliases

    % MULTIPLEVALUESTRING
    {ok, F_ExecInst} = erlyfix_utils:lookup(P, {field, by_name, 'ExecInst' }),
    ok = erlyfix_fields:validate(<<"String">>, F_ExecInst, H),
    <<"String">> = erlyfix_fields:convert(<<"String">>, F_ExecInst),
    % EXCHANGE
    {ok, F_LastMkt} = erlyfix_utils:lookup(P, {field, by_name, 'LastMkt' }),
    ok = erlyfix_fields:validate(<<"String">>, F_LastMkt, H),
    <<"String">> = erlyfix_fields:convert(<<"String">>, F_LastMkt),
    % SEQNUM
    {ok, F_MsgSeqNum} = erlyfix_utils:lookup(P, {field, by_name, 'MsgSeqNum' }),
    ok = erlyfix_fields:validate(<<"123">>, F_MsgSeqNum, H),
    123 = erlyfix_fields:convert(<<"123">>, F_MsgSeqNum),
    % NUMINGROUP
    {ok, F_NoExecs} = erlyfix_utils:lookup(P, {field, by_name, 'NoExecs' }),
    ok = erlyfix_fields:validate(<<"123">>, F_NoExecs, H),
    123 = erlyfix_fields:convert(<<"123">>, F_NoExecs),
    % ATM
    {ok, F_Concession} = erlyfix_utils:lookup(P, {field, by_name, 'Concession' }),
    ok = erlyfix_fields:validate(<<"123">>, F_Concession, H),
    123 = erlyfix_fields:convert(<<"123">>, F_Concession),
    % PERCENTAGE
    {ok, F_LegRepurchaseRate} = erlyfix_utils:lookup(P, {field, by_name, 'LegRepurchaseRate' }),
    ok = erlyfix_fields:validate(<<"123">>, F_LegRepurchaseRate, H),
    123 = erlyfix_fields:convert(<<"123">>, F_LegRepurchaseRate),
    % PRICE
    {ok, F_BasisFeaturePrice} = erlyfix_utils:lookup(P, {field, by_name, 'BasisFeaturePrice' }),
    ok = erlyfix_fields:validate(<<"123">>, F_BasisFeaturePrice, H),
    123 = erlyfix_fields:convert(<<"123">>, F_BasisFeaturePrice),
    % QTY
    {ok, F_DefBidSize} = erlyfix_utils:lookup(P, {field, by_name, 'DefBidSize' }),
    ok = erlyfix_fields:validate(<<"123">>, F_DefBidSize, H),
    123 = erlyfix_fields:convert(<<"123">>, F_DefBidSize),
    % PRICEOFFSET
    {ok, F_PriceImprovement} = erlyfix_utils:lookup(P, {field, by_name, 'PriceImprovement' }),
    ok = erlyfix_fields:validate(<<"123">>, F_PriceImprovement, H),
    123 = erlyfix_fields:convert(<<"123">>, F_PriceImprovement),
    % UTCDATEONLY
    {ok, F_MDEntryDate} = erlyfix_utils:lookup(P, {field, by_name, 'MDEntryDate' }),
    ok = erlyfix_fields:validate(<<"19981231">>, F_MDEntryDate, H),
    ?assertEqual(
        #fix_date { year = 1998, month = 12, day = 31},
        erlyfix_fields:convert(<<"19981231">>, F_MDEntryDate)
    ).

