-module(erlyfix_test05).
-include_lib("eunit/include/eunit.hrl").
-include("erlyfix_records.hrl").

-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

load() ->
    DirName = "priv/protocols/",
    Path = DirName ++ "FIX44.xml",
    erlyfix_protocol:load(Path).

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
            {field, GetField('BeginString'), "FIX.4.4"},
            {field, GetField('BodyLength'), 90},
            {field, GetField('MsgType'), "A"},
            {field, GetField('SenderCompID'), "me"},
            {field, GetField('TargetCompID'), "you"},
            {field, GetField('MsgSeqNum'), "1"},
            {field, GetField('SendingTime'), "20090107-18:15:16"},
        {finish,header},
        {start,body,{}},
            {field, GetField('EncryptMethod'), "0"},
            {field, GetField('HeartBtInt'), "60"},
            {start,group,{'NoMsgTypes',2}},
                {field, GetField('RefMsgType'), "abc"},
                {field, GetField('MsgDirection'), "S"},
                {field, GetField('RefMsgType'), "def"},
                {field, GetField('MsgDirection'), "R"},
            {finish,group},
        {finish,body},
        {start,trailer,{}},
            {field, GetField('CheckSum'), 229},
        {finish,trailer}
    ],
    ?DEBUG(Markup_Expected),
    ?DEBUG(TagsMarkup),
    ?assertEqual(Markup_Expected, TagsMarkup).

