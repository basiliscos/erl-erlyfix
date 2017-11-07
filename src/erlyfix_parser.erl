-module(erlyfix_parser).
-include("erlyfix_records.hrl").

-export([parse/2]).
-define(SEPARATOR, <<1:8>>).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(context, {protocol, data}).

pull_tagno({List, Size}) ->
    case re:split(List, <<"=">>, [{parts, 2}, {return, list}]) of
        [TagNoList, Rest] ->
            TagNoSize = length(TagNoList),
            case re:run(TagNoList, <<"\\d+">>) of
                {match, [{0, TagNoSize}]} ->
                    {ok, list_to_integer(TagNoList), TagNoSize, {Rest, Size-(TagNoSize+1) }};
                _SomethingElse ->
                    Err = io_lib:format("Tag '~w' is not a number", [TagNoList]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        [List] -> not_found
    end.

pull_tagvalue({List, Size}, any_size) ->
    case re:split(List, <<1>>, [{parts, 2}, {return, list}]) of
        [TagValue, Rest] ->
            TagValueSize = length(TagValue),
            {ok, TagValue, TagValueSize, {Rest, Size - (TagValueSize + 1)}};
        [List] -> not_found
    end;
pull_tagvalue({List, Size}, TagValueSize) ->
    case (Size + 1) < TagValueSize of
        true -> not_found;
        false ->
            {TagValue, Rest0} = lists:split(TagValueSize, List),
            Rest1 = lists:nthtail(1, Rest0),
            {ok, TagValue, TagValueSize, {Rest1, Size - (TagValueSize + 1)}}
    end.

parse_tagvalue(Data, F) ->
    N = F#field.number,
    case pull_tagno(Data) of
        {ok, N, TagNoSize, Rest0} ->
            case pull_tagvalue(Rest0, any_size) of
                {ok, TagValue, TagValueSize, Rest1} ->
                    case erlyfix_fields:validate({TagValue, TagValueSize}, F) of
                        ok    -> {ok, TagValue, TagNoSize + TagValueSize + 1, Rest1};
                        error ->
                            Err = io_lib:format("Value '~s' does pass validatation for field '~s'",
                                [TagValue, F#field.name]),
                            Reason = erlang:iolist_to_binary(Err),
                            {error, Reason}
                    end;
                SomethingElse -> SomethingElse
            end;
        {ok, K, _TagNoSize, _Rest} ->
            Err = io_lib:format("Tag mismatch. Expected: '~w', got '~w'", [N, K]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason};
        SomethingElse -> SomethingElse
    end.

tagvalue_size(F, Acc) ->
    case F#field.type of
        'DATA' ->
            case Acc of
                [H | _T] ->
                    {F_prev, Value_prev, _TagSize} = H,
                    case F_prev#field.type of
                        'LENGTH' ->
                            DataSize = erlyfix_fields:convert(Value_prev, F_prev),
                            {ok, DataSize};
                        _OtherType ->
                            Err = io_lib:format("Expected that 'LENGTH' tag should precede the"
                                ++ " current 'DATA', meanwhile got '~w'", [F_prev#field.type]),
                            Reason = erlang:iolist_to_binary(Err),
                            {error, Reason}
                    end;
                _Empty ->
                    Err = io_lib:format("Current 'DATA' tag is not preceded with 'LENGTH' tag"),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        _AnyOtherType -> {ok, any_size}
    end.


parse_tagpair(Data, P, Acc) ->
    case pull_tagno(Data) of
        {ok, FieldNo, TagNoSize, Rest0} ->
            case erlyfix_protocol:lookup(P, {field, by_number, FieldNo}) of
                {ok, F} ->
                    case tagvalue_size(F, Acc) of
                        {ok, ExpectdTagSize} ->
                            case pull_tagvalue(Rest0, ExpectdTagSize) of
                                {ok, TagValue, TagValueSize, Rest1} ->
                                    case erlyfix_fields:validate({TagValue, TagValueSize}, F) of
                                        ok    -> {ok, F, TagValue, TagNoSize + TagValueSize, Rest1};
                                        error ->
                                            Str = "Value '~s' does pass validatation for field '~s'",
                                            Err = io_lib:format(Str, [TagValue, F#field.name]),
                                            Reason = erlang:iolist_to_binary(Err),
                                            {error, Reason}
                                    end;
                                SomethingElse -> SomethingElse
                            end;
                        SomethingElse -> SomethingElse
                    end;
                not_found ->
                    Err = io_lib:format("Unknown field '~B'", [FieldNo]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        SomethingElse -> SomethingElse
    end.

parse_introduction(Data, #context{protocol = P, data = _D}, Acc)->
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'BeginString' }),
    Major = P#protocol.protocol_version#protocol_version.major,
    Minor = P#protocol.protocol_version#protocol_version.minor,
    BeginString = lists:flatten(io_lib:format("FIX.~B.~B", [Major, Minor])),

    case parse_tagvalue(Data, F) of
        {error, Reason} -> {error, Reason};
        not_found ->
            LengthMin = byte_size(integer_to_binary(F#field.number))
                + 1 % '=' aka separator
                + byte_size(BeginString),
            % ?DEBUG(Data),
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
        {ok, BinaryValue, Size, Rest} ->
            % It should not throw, as it already bypassed validation
            BodyLength = erlyfix_fields:convert(BinaryValue, F),
            {ok, [{F, BodyLength, Size} | Acc], Rest}
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
                    Checksum = list_to_integer(Value),
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
             try list_to_existing_atom(MsgTypeValue) of
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
                error:badarg ->
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
    case parse_tagpair(Body, P, Acc) of
        {ok, F, Value, TagSize, Rest} ->
            parse_tags(Rest, P, [ {F, Value, TagSize} | Acc ]);
        _OtherResult -> _OtherResult
    end.

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

classify_item([{F, V, _Size} | T], C, Acc0) when element(1, C) =:= field ->
    Acc1 = [{field, F, V} | Acc0],
    {ok, T, Acc1};
classify_item([{_F, V, _Size} | T], C, Acc0) when element(1, C) =:= group ->
    Count = list_to_integer(V),
    ScopeCTX = {C#group.name, Count},
    C4F = C#group.composite4field,
    MC = C#group.mandatoryComposites,
    case start_classify_scope(T, {group, ScopeCTX, C4F, MC}) of
        {ok, Acc1, L1} -> {ok, L1, lists:reverse(Acc1) ++ Acc0};
        _OtherResult -> _OtherResult
    end;
classify_item(L, C, Acc0) when element(1, C) =:= component ->
    C4F = C#component.composite4field,
    MC = C#component.mandatoryComposites,
    ScopeCTX = {C#component.name},
    case start_classify_scope(L, {component, ScopeCTX, C4F, MC}) of
        {ok, Acc1, L1} -> {ok, L1, lists:reverse(Acc1) ++ Acc0};
        _OtherResult -> _OtherResult
    end.

classify_scope([], Current, MandatoryLeft, Acc) ->
    finish_classify_scope([], Current, MandatoryLeft, Acc);
classify_scope([H | _T] = L, {_Scope, C4F} = Current, MandatoryLeft, Acc) ->
    %?DEBUG(H),
    F = element(1, H),
    %?DEBUG(F),
    % ?DEBUG(F#field.name),
    case maps:find(F, C4F) of
        {ok, C} ->
            C_name = erlyfix_composite:name(C),
            % ?DEBUG(element(1, C)),
            % ?DEBUG(C_name),
            MandatoryLeft1 = maps:remove(C_name, MandatoryLeft),
            case classify_item(L, C, Acc) of
                {ok, L1, Acc1} -> classify_scope(L1, Current, MandatoryLeft1, Acc1);
                _OtherResult -> _OtherResult
            end;
        error -> finish_classify_scope(L, Current, MandatoryLeft, Acc)
    end.
start_classify_scope(List, {Scope, ScopeCTX, C4F, MC}) ->
    % ?DEBUG(["Start "] ++ [atom_to_list(Scope)]),
    classify_scope(List, {Scope, C4F}, MC, [{start, Scope, ScopeCTX}]).


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

classify_message(M, P, L) ->
    H = P#protocol.header,
    T = P#protocol.trailer,
    Scopes = [
        {header, {}, H#header.composite4field, H#header.mandatoryComposites},
        {body, {}, M#message.composite4field, M#message.mandatoryComposites},
        {trailer, {}, T#trailer.composite4field, T#trailer.mandatoryComposites}
    ],
    classify(L, Scopes, []).

parse(Data, P) ->
    case parse_managed_fields(Data, P) of
        {ok, Acc, RestOfBody} ->
            [Message, TagMessageType, TagChecksum, {next_message, RestData} | Acc0 ] = Acc,
            % reconstruct original tags sequence
            Acc1 = [TagMessageType | Acc0],
            case parse_tags(RestOfBody, P, Acc1) of
                {ok, Acc2} ->
                    %?DEBUG(TagChecksum),
                    % reconstruct original tags sequence
                    Acc3 = lists:reverse([TagChecksum | Acc2]),
                    case classify_message(Message, P, Acc3) of
                        {ok, Acc4} -> {ok, Message#message.name, Acc4, RestData};
                        _OtherResult -> _OtherResult
                    end;
                _OtherResult -> _OtherResult
            end;
        _OtherResult -> _OtherResult
    end.
