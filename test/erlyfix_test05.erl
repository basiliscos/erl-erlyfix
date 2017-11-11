-module(erlyfix_test05).
-include_lib("eunit/include/eunit.hrl").
-include("include/erlyfix.hrl").

-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

load() ->
    Path = "test/FIX44.xml",
    {ok, P} = erlyfix_protocol:load(Path),
    P.

regular_message_parse_test() ->
    P = load(),
    M = <<"8=FIX.4.4", 1, "9=90", 1, "35=A", 1, "49=me", 1, "56=you", 1,
        "34=1", 1, "52=20090107-18:15:16", 1, "98=0", 1, "108=60", 1, "384=2", 1,
        "372=abc", 1, "385=S", 1, "372=def", 1, "385=R", 1, "10=229", 1
    >>,
    IOList = binary_to_list(M),
    Size = length(IOList),
    ?DEBUG(erlyfix_parser:parse({IOList, Size}, P)),
    {ok, 'Logon', TagsMarkup, {"", 0}} = erlyfix_parser:parse({IOList, Size}, P),
    GetField = fun(Name) ->
        {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, Name}),
        F
    end,
    Markup_Expected = [
        {start, header,{}},
            {field, 'BeginString', GetField('BeginString'), "FIX.4.4"},
            {field, 'BodyLength', GetField('BodyLength'), 90},
            {field, 'MsgType', GetField('MsgType'), "A"},
            {field, 'SenderCompID', GetField('SenderCompID'), "me"},
            {field, 'TargetCompID', GetField('TargetCompID'), "you"},
            {field, 'MsgSeqNum', GetField('MsgSeqNum'), "1"},
            {field, 'SendingTime', GetField('SendingTime'), "20090107-18:15:16"},
        {finish,header},
        {start,body,{}},
            {field, 'EncryptMethod', GetField('EncryptMethod'), "0"},
            {field, 'HeartBtInt', GetField('HeartBtInt'), "60"},
            {start,group,{'NoMsgTypes',2}},
                {field, 'RefMsgType', GetField('RefMsgType'), "abc"},
                {field, 'MsgDirection', GetField('MsgDirection'), "S"},
                {field, 'RefMsgType', GetField('RefMsgType'), "def"},
                {field, 'MsgDirection', GetField('MsgDirection'), "R"},
            {finish,group},
        {finish,body},
        {start,trailer,{}},
            {field, 'CheckSum', GetField('CheckSum'), 229},
        {finish,trailer}
    ],
    % ?DEBUG(Markup_Expected),
    % ?DEBUG(TagsMarkup),
    ?assertEqual(Markup_Expected, TagsMarkup).

