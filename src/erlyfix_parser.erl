-module(erlyfix_parser).
-include("erlyfix_records.hrl").

-export([parse/2]).
-define(SEPARATOR, <<1:8>>).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(context, {protocol, data}).

parse_tagpair(Data) ->
    case re:split(Data, <<1>>, [{parts, 2}, {return, binary}]) of
        [Pair, Rest] ->
            case re:split(Pair, <<"=">>, [{parts, 2}, {return, binary}]) of
                [Tag, Value] ->
                    TagSize = byte_size(Tag),
                    case re:run(Tag, <<"\\d+">>) of
                        {match, [{0, TagSize}]} ->
                            {ok, {binary_to_integer(Tag), Value}, Rest};
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
        [Data] -> {not_found, Data}
    end.

parse_tagvalue(Data, F) ->
    N = F#field.number,
    case parse_tagpair(Data) of
        {ok, {Tag, BinaryValue}, Rest} ->
            case Tag =:= N of
                false ->
                    Err = io_lib:format("Tag mismatch. Expected: '~w', got '~w'", [N, Tag]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason};
                true ->
                    case erlyfix_fields:convert(BinaryValue, F) of
                        {ok, Value} -> {ok, Value, Rest};
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
        {not_found, Data} ->
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
        {ok, Value, Rest} ->
            case Value of
                BeginString ->
                    {ok, [{F, Value} | Acc], Rest};
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
        {not_found, Data} -> no_enough_data;
        {ok, BodyLength, Rest} -> {ok, [{F, BodyLength} | Acc], Rest}
    end.

confirm_length(Data, _Ctx, Acc) ->
    [{_F_Length, BodyLength} | _T] = Acc,
    case byte_size(Data) >= BodyLength of
        true -> {ok, Acc, Data};
        false -> no_enough_data
    end.

extract_checksum(Data, #context{protocol = P, data = _D}, Acc) ->
    [{_F_length, BodyLength} | Acc0] = Acc,
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'CheckSum' }),
    TagLength = iolist_size(io_lib:format(<<"~B=xxx", 1>>, [F#field.number])),
    <<Body:BodyLength/binary, TagData:TagLength/binary, NextMessage/binary>> = Data,
    case parse_tagvalue(TagData, F) of
        {error, Reason} -> {error, Reason};
        {not_found, Data} ->
            Err = io_lib:format("Checksum tag not found in '~s'", [TagData]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason};
        {ok, Value, Rest} ->
            case re:run(Value, <<"\\d{3}">>) of
                {match,[{0,3}]} ->
                    Checksum = binary_to_integer(Value),
                    {ok, [{F, Checksum, TagLength}, {next_message, NextMessage} | Acc], Body};
                _SomethingElse ->
                    Err = io_lib:format("Checksum format mismatch: 's' ",[Value]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end
    end.

confirm_checksum(Body, #context{protocol = P, data = Data}, Acc) ->
    [{_F, Checksum, ChecksumSize} | Acc0 ] = Acc,
    TotalSize = byte_size(Data),
    CheckSummedSize = TotalSize - ChecksumSize,
    <<SubjectData:CheckSummedSize/binary, _Tag:ChecksumSize/binary>> = Data,
    ActualChecksum = erlyfix_utils:checksum(binary_to_list(SubjectData)),
    case Checksum =:= ActualChecksum of
        true ->
            {ok, Acc, Body};
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
        {ok, MsgTypeValue, RestOfBody} ->
             try binary_to_existing_atom(MsgTypeValue, latin1) of
                MsgType ->
                    case erlyfix_protocol:lookup(P, {message, by_type, MsgType }) of
                        {ok, Message} ->
                            % special case, we put Message on top of accumulator
                            {ok, [Message, {F, MsgTypeValue} | Acc], RestOfBody};
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

parse_tags(<<"">>, _P, Acc) -> Acc;
parse_tags(Body, P, Acc) ->
    case parse_tagpair(Body) of
        {ok, {FieldNo, Value}, Rest} ->
            case erlyfix_protocol:lookup(P, {field, by_number, FieldNo}) of
                {ok, F} -> parse_tags(Rest, P, [ {F, Value} | Acc ]);
                not_found ->
                    Err = io_lib:format("Unknown field '~B'", [FieldNo]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        _OtherResult -> _OtherResult
    end.

parse(Data, P) ->
    case parse_managed_fields(Data, P) of
        {ok, Acc, RestOfBody} ->
            [Message, TagMessageType, TagChecksum, {next_message, RestData} | Acc0 ] = Acc,
            % reconstruct original tags sequence
            Acc1 = [TagMessageType | Acc0],
            case parse_tags(RestOfBody, P, Acc1) of
                Acc2 ->
                    % reconstruct original tags sequence
                    {[ TagChecksum | Acc2], RestData};
                _OtherResult -> _OtherResult
            end;
        _OtherResult -> _OtherResult
    end.
