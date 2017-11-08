-module(erlyfix_test08).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    {ok, P} = erlyfix_protocol:load(Path),
    P.

as_list(L) -> {L, length(L)}.

parsing_error_test() ->
    P = load(),
    {error, R1} =  erlyfix_parser:parse({"garbage-garbage", 16}, P),
    ?assertEqual(<<"FIX header has not been found in sequence '[103,97,114,98,97,103,101,45,103,97,114,98,97,103,101]'">>, R1),

    no_enough_data = erlyfix_parser:parse({"abc", 3}, P),

    {error, R2} =  erlyfix_parser:parse({"8=FIX.4.2" ++ [1], 11}, P),
    ?assertEqual(<<"FIX header mismatch. Expected: 'FIX.4.4', got 'FIX.4.2'">>, R2),

    {error, R3} =  erlyfix_parser:parse({"9=FIX.4.2" ++ [1], 11}, P),
    ?assertEqual(<<"Tag mismatch. Expected: '8' (BeginString), got '9'">>, R3),

    {error, R4} =  erlyfix_parser:parse({"8=FIX.4.4" ++ [1] ++ "9=length!" ++ [1], 22}, P),
    ?assertEqual(<<"Value '[108,101,110,103,116,104,33]' does pass validatation for field 9 ('BodyLength')">>, R4),

    no_enough_data = erlyfix_parser:parse({"8=FIX.4.4" ++ [1] ++ "9=", 13}, P),
    no_enough_data = erlyfix_parser:parse({"8=FIX.4.4" ++ [1] ++ "9=25" ++ [1], 16}, P),
    no_enough_data = erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=0" ++ [1] ++ "11=1" ++ [1]), P),

    {error, R5} =  erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=5" ++ [1] ++ "9=15" ++ [1] ++ "xxxxxx" ++ [1]), P),
    ?assertEqual(<<"Checksum tag (10) not found in sequence '[120,120,120,120,120,120,1]'">>, R5),

    {error, R6} =  erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=5" ++ [1] ++ "9=15" ++ [1] ++ "10=xxx" ++ [1]), P),
    ?assertEqual(<<"Checksum format mismatch: '[120,120,120]' is not 3-digit string">>, R6),

    {error, R7} =  erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=5" ++ [1] ++ "9=15" ++ [1] ++ "10=" ++ [1,1,1,1]), P),
    ?assertEqual(<<"Value '[1,1,1]' does pass validatation for field 10 ('CheckSum')">>, R7),

    {error, R8} =  erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=5" ++ [1] ++ "9=15" ++ [1] ++ "10=123" ++ [1]), P),
    % ?debugFmt("~p", [R8]),
    ?assertEqual(<<"Checksum mismatch. Expected: '123', got '170'">>, R8),

    {error, R9} =  erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=7" ++ [1] ++ "35=ZZZ" ++ [1] ++ "10=131" ++ [1]), P),
    ?assertEqual(<<"Unknown message type 'ZZZ'">>, R9),

    {error, R10} =  erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=7" ++ [1] ++ "35=app" ++ [1] ++ "10=182" ++ [1]), P),
    ?assertEqual(<<"Unknown message type 'app'">>, R10),

    {error, R11} =  erlyfix_parser:parse({"X=FIX.4.4" ++ [1], 11}, P),
    ?assertEqual(<<"Tag '[88]' is not a number">>, R11),

    {error, R12} =  erlyfix_parser:parse(as_list("8=FIX.4.4" ++ [1] ++ "9=11" ++ [1] ++
        "35=A" ++ [1] ++  "213=1" ++ [1] ++ "10=230" ++ [1]), P),
    ?assertEqual(<<"Expected that type 'LENGTH' tag should precede the current ",
        "'DATA' field type, meanwhile got field 35 ('MsgType') of type 'STRING'">>, R12),

    {error, R13} =  erlyfix_parser:parse(as_list("8=FIX.4.4" ++ [1] ++ "9=11" ++ [1] ++
        "35=A" ++ [1] ++  "xxx=1" ++ [1] ++ "10=184" ++ [1]), P),
    ?assertEqual(<<"Tag '[120,120,120]' is not a number">>, R13),

    {error, R14} =  erlyfix_parser:parse(as_list("8=FIX.4.4" ++ [1] ++ "9=11" ++ [1] ++
        "35=A" ++ [1] ++  "999=1" ++ [1] ++ "10=251" ++ [1]), P),
    ?assertEqual(<<"Unknown field '999'">>, R14),

    {error, R15} =  erlyfix_parser:parse(as_list("8=FIX.4.4" ++ [1] ++ "9=11" ++ [1] ++
        "35=A" ++ [1] ++  "212=a" ++ [1] ++ "10=021" ++ [1]), P),
    ?assertEqual(<<"Value 'a' does pass validatation for field 'XmlDataLen'">>, R15),

    {error, R16} = erlyfix_parser:parse(as_list("8=FIX.4.4" ++ [1] ++ "9=19" ++ [1] ++
        "35=A" ++ [1] ++  "212=100" ++ [1] ++ "213=1" ++ [1] ++ "10=082" ++ [1]), P),
    ?assertEqual(<<"Canot extract tag value for field 213 ('XmlData')">>, R16),

    {error, R17} = erlyfix_parser:parse(as_list(
        "8=FIX.4.4" ++ [1] ++ "9=7" ++ [1] ++ "xx=ZZZ" ++ [1] ++ "10=011" ++ [1]), P),
    % ?debugFmt("~p", [R17]),
    ?assertEqual(<<"Tag '[120,120]' is not a number">>, R17).
