-module(erlyfix_parser).
-include("erlyfix_records.hrl").

-export([parse/2]).
-define(SEPARATOR, <<1:8>>).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(context, {protocol, data}).

parse_tagpair({List, Size}) ->
    case re:split(List, <<1>>, [{parts, 2}, {return, list}]) of
        [Pair, Rest] ->
            case re:split(Pair, <<"=">>, [{parts, 2}, {return, iodata}]) of
                [Tag, Value] ->
                    TagSize = iolist_size(Tag),
                    case re:run(Tag, <<"\\d+">>) of
                        {match, [{0, TagSize}]} ->
                            %?DEBUG(Rest),
                            PairSize = iolist_size(Pair) + 1,
                            {ok, {binary_to_integer(Tag), Value, PairSize}, {Rest, Size - PairSize}};
                        _SomethingElse ->
                            Err = io_lib:format("Tag '~s' is not a number", [Tag]),
                            Reason = erlang:iolist_to_binary(Err),
                            {error, Reason}
                    end;
                [Rest] ->
                    Err = io_lib:format("Sequence '~s' does not match tagpair", [Pair]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        [List] -> not_found
    end.

parse_tagvalue(Data, F) ->
    N = F#field.number,
    case parse_tagpair(Data) of
        {ok, {Tag, BinaryValue, Size}, Rest} ->
            %?DEBUG(Rest),
            case Tag =:= N of
                false ->
                    Err = io_lib:format("Tag mismatch. Expected: '~w', got '~w'", [N, Tag]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason};
                true ->
                    case erlyfix_fields:convert(BinaryValue, F) of
                        {ok, Value} -> {ok, Value, Size, Rest};
                        {error, Reason} -> {error, Reason}
                    end
            end;
        SomethingElse -> SomethingElse
    end.

parse_introduction(Data, #context{protocol = P, data = _D}, Acc)->
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'BeginString' }),
    Major = P#protocol.protocol_version#protocol_version.major,
    Minor = P#protocol.protocol_version#protocol_version.minor,
    BeginString = erlang:iolist_to_binary(io_lib:format("FIX.~B.~B", [Major, Minor])),

    case parse_tagvalue(Data, F) of
        {error, Reason} -> {error, Reason};
        not_found ->
            LengthMin = byte_size(integer_to_binary(F#field.number))
                + 1 % '=' aka separator
                + byte_size(BeginString),
            case byte_size(Data) >= LengthMin of
                false -> no_enough_data;
                true ->
                    Err = io_lib:format("Sequence '~s' does not FIX header", [Data]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        {ok, Value, Size, Rest} ->
            case Value of
                BeginString ->
                    {ok, [{F, Value, Size} | Acc], Rest};
                SomethingElse ->
                    Err = io_lib:format("FIX header mismatch. Expected: '~s', got '~s'",
                        [BeginString, SomethingElse]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end
    end.

parse_length(Data, #context{protocol = P, data = _D}, Acc) ->
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'BodyLength' }),
    case parse_tagvalue(Data, F) of
        {error, Reason} -> {error, Reason};
        not_found -> no_enough_data;
        {ok, BodyLength, Size, Rest} -> {ok, [{F, BodyLength, Size} | Acc], Rest}
    end.

confirm_length({_List, Size} = Data, _Ctx, Acc) ->
    [{_F_Length, BodyLength, _TagSize} | _T] = Acc,
    case Size >= BodyLength of
        true -> {ok, Acc, Data};
        false -> no_enough_data
    end.

extract_checksum({List, Size}, #context{protocol = P, data = _D}, Acc) ->
    [{_F_length, BodyLength, _F_Lengt_Size} | _T] = Acc,
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'CheckSum' }),
    TagLength = iolist_size(io_lib:format(<<"~B=xxx", 1>>, [F#field.number])),
    {Body, Tail} = lists:split(BodyLength, List),
    {TagData, NextMessage} = lists:split(TagLength, Tail),
    case parse_tagvalue({TagData, TagLength}, F) of
        {error, Reason} -> {error, Reason};
        not_found ->
            Err = io_lib:format("Checksum tag not found in '~s'", [TagData]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason};
        {ok, Value, _TagLength, _Rest} ->
            case re:run(Value, <<"\\d{3}">>) of
                {match,[{0,3}]} ->
                    Checksum = binary_to_integer(Value),
                    BytesLeft = Size - (BodyLength + TagLength),
                    MessageBody = {Body, BodyLength},
                    {ok, [{F, Checksum, TagLength}, {next_message, {NextMessage, BytesLeft}} | Acc], MessageBody};
                _SomethingElse ->
                    Err = io_lib:format("Checksum format mismatch: '~s' ",[Value]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end
    end.

confirm_checksum(Body, #context{protocol = _P, data = {List, Size}}, Acc) ->
    [{F, Checksum, ChecksumSize} | Acc0 ] = Acc,
    CheckSummedSize = Size - ChecksumSize,
    {SubjectData, _Tail} = lists:split(CheckSummedSize, List),
    ActualChecksum = erlyfix_utils:checksum(SubjectData),
    case Checksum =:= ActualChecksum of
        true ->
            {ok, [{F, Checksum, ChecksumSize} | Acc0], Body};
        false ->
            Err = io_lib:format("Checksum mismatch. Expected: '~B', got '~B'",
                [Checksum, ActualChecksum]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason}
    end.

extract_message_type(Body, #context{protocol = P, data = _D}, Acc) ->
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'MsgType' }),
    case parse_tagvalue(Body, F) of
        {error, Reason} -> {error, Reason};
        {not_found, Body} -> no_enough_data;
        {ok, MsgTypeValue, TagSize, RestOfBody} ->
             try binary_to_existing_atom(MsgTypeValue, latin1) of
                MsgType ->
                    case erlyfix_protocol:lookup(P, {message, by_type, MsgType }) of
                        {ok, Message} ->
                            % special case, we put Message on top of accumulator
                            {ok, [Message, {F, MsgTypeValue, TagSize} | Acc], RestOfBody};
                        not_found ->
                            Err = io_lib:format("Unknown message type '~s'", [MsgTypeValue]),
                            Reason = erlang:iolist_to_binary(Err),
                            {error, Reason}
                    end
            catch
                badarg ->
                    Err = io_lib:format("Unknown message type '~s'", [MsgTypeValue]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end
    end.


parse_pipeline(Data, _Ctx, Acc, []) -> {ok, Acc, Data};
parse_pipeline(Data, Ctx, Acc, [F | Fs]) ->
    case F(Data, Ctx, Acc) of
        {ok, NewAcc, Rest} -> parse_pipeline(Rest, Ctx, NewAcc, Fs);
        _OtherResult -> _OtherResult
    end.

parse_managed_fields(Data, P) ->
    Fs = [
        fun parse_introduction/3,
        fun parse_length/3,
        fun confirm_length/3,
        fun extract_checksum/3,
        fun confirm_checksum/3,
        fun extract_message_type/3
    ],
    parse_pipeline(Data, #context{ protocol = P, data = Data}, [], Fs).

parse_tags({_L, 0}, _P, Acc) -> {ok, Acc};
parse_tags(Body, P, Acc) ->
    case parse_tagpair(Body) of
        {ok, {FieldNo, Value, TagSize} = Tag, Rest} ->
            case erlyfix_protocol:lookup(P, {field, by_number, FieldNo}) of
                {ok, F} -> parse_tags(Rest, P, [ {F, Value, TagSize} | Acc ]);
                not_found ->
                    Err = io_lib:format("Unknown field '~B'", [FieldNo]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        _OtherResult -> _OtherResult
    end.

classify_group(L, G, V) ->
    Count = binary_to_integer(V),
    ScopeCTX = {G#group.name, Count},
    C4N = G#group.composite4name,
    MC = G#group.mandatoryComposites,
    start_classify_scope(L, {group, ScopeCTX, C4N, MC}).

finish_classify_scope(L, {Scope, _C4N}, MandatoryLeft, Acc) ->
    case maps:size(MandatoryLeft) of
        0 ->
            Acc1 = lists:reverse([{finish, Scope} | Acc ]),
            % ?DEBUG(["Finish "] ++ [atom_to_list(Scope)]),
            {ok, Acc1, L};
        _N ->
            [Name | _T ] = maps:keys(MandatoryLeft),
            Err = io_lib:format("Missing mandatory '~s' for '~s'", [Name, Scope]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason}
    end.

classify_scope([], Current, MandatoryLeft, Acc) ->
    finish_classify_scope([], Current, MandatoryLeft, Acc);
classify_scope([H | T] = L, {_Scope, C4N} = Current, MandatoryLeft, Acc) ->
    %?DEBUG(H),
    {F, V, _Size} = H,
    %?DEBUG(F),
    %?DEBUG(F#field.name),
    case maps:find(F#field.name, C4N) of
        {ok, F} ->
            MandatoryLeft2 = maps:remove(F#field.name, MandatoryLeft),
            classify_scope(T, Current, MandatoryLeft2, [{field, F, V} | Acc]);
        {ok, G} when element(1, G) =:= group ->
            MandatoryLeft2 = maps:remove(G#group.name, MandatoryLeft),
            case classify_group(T, G, V) of
                {ok, AccG, L2} ->
                    Acc2 = lists:reverse(AccG) ++ Acc,
                    classify_scope(L2, Current, MandatoryLeft2, Acc2);
                _OtherResult -> _OtherResult
            end;
        error -> finish_classify_scope(L, Current, MandatoryLeft, Acc)
    end.
start_classify_scope(List, {Scope, ScopeCTX, C4N, MC}) ->
    % ?DEBUG(["Start "] ++ [atom_to_list(Scope)]),
    classify_scope(List, {Scope, C4N}, MC, [{start, Scope, ScopeCTX}]).


classify([], _Candidates, Acc) ->
    Acc1 = lists:reverse(Acc),
    Acc2 = lists:flatten(Acc1),
    {ok, Acc2};
classify([H | _T], [], _Acc) ->
    {F, _V} = H,
    Err = io_lib:format("Unknown filed '~s'", [F#field.name]),
    Reason = erlang:iolist_to_binary(Err),
    {error, Reason};
classify(L, [Scope | ScopeTail], Acc) ->
    case start_classify_scope(L, Scope) of
        {ok, InnerAcc, L1} ->
            Acc1 = [InnerAcc | Acc],
            classify(L1, ScopeTail, Acc1);
        _OtherResult -> _OtherResult
    end.

parse(Data, P) ->
    case parse_managed_fields(Data, P) of
        {ok, Acc, RestOfBody} ->
            [Message, TagMessageType, TagChecksum, {next_message, RestData} | Acc0 ] = Acc,
            % reconstruct original tags sequence
            Acc1 = [TagMessageType | Acc0],
            case parse_tags(RestOfBody, P, Acc1) of
                {ok, Acc2} ->
                    %?DEBUG(Acc2),
                    % reconstruct original tags sequence
                    Acc3 = lists:reverse([TagChecksum | Acc2]),
                    H = P#protocol.header,
                    T = P#protocol.trailer,
                    Scopes = [
                        {header, {}, H#header.composite4name, H#header.mandatoryComposites},
                        {body, {}, Message#message.composite4name, Message#message.mandatoryComposites},
                        {trailer, {}, T#trailer.composite4name, T#trailer.mandatoryComposites}
                    ],
                    case classify(Acc3, Scopes, []) of
                        {ok, Acc4} -> {ok, Message#message.name, Acc4, RestData};
                        _OtherResult -> _OtherResult
                    end;
                _OtherResult -> _OtherResult
            end;
        _OtherResult -> _OtherResult
    end.
