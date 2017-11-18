-module(erlyfix_parser).
-include("include/erlyfix.hrl").

-export([parse/2]).
-define(SEPARATOR, <<1:8>>).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(context, {protocol, data}).

pull_tagno(Data) ->
    case re:split(Data, <<"=">>, [{parts, 2}, {return, binary}]) of
        [TagNo, Rest] ->
            TagNoSize = byte_size(TagNo),
            case re:run(TagNo, <<"\\d+">>) of
                {match, [{0, TagNoSize}]} ->
                    {ok, binary_to_integer(TagNo), TagNoSize, Rest};
                _SomethingElse ->
                    Err = io_lib:format("Tag '~w' is not a number", [TagNo]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        [Data] -> not_found
    end.

pull_tagvalue(Data, any_size) ->
    case re:split(Data, <<1>>, [{parts, 2}, {return, binary}]) of
        [TagValue, Rest] ->
            {ok, TagValue, Rest};
        [Data] -> not_found
    end;
pull_tagvalue(Data, TagValueSize) ->
    case (byte_size(Data) + 1) < TagValueSize of
        true -> not_found;
        false ->
            <<TagValue:TagValueSize/binary, _Separtor:1/binary, Rest/binary>> = Data,
            {ok, TagValue, Rest}
    end.

parse_tagvalue(Data, F, ValueSize) ->
    N = F#field.number,
    case pull_tagno(Data) of
        {ok, N, TagNoSize, Rest0} ->
            case pull_tagvalue(Rest0, ValueSize) of
                {ok, TagValue, Rest1} ->
                    case erlyfix_fields:validate(TagValue, F) of
                        ok    -> {ok, TagValue, TagNoSize + byte_size(TagValue) + 1, Rest1};
                        error ->
                            Err = io_lib:format("Value '~w' does pass validatation for field ~B ('~s')",
                                [TagValue, N, F#field.name]),
                            Reason = erlang:iolist_to_binary(Err),
                            {error, Reason}
                    end;
                SomethingElse -> SomethingElse
            end;
        {ok, K, _TagNoSize, _Rest} ->
            Err = io_lib:format("Tag mismatch. Expected: '~w' (~s), got '~w'", [N, F#field.name, K]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason};
        SomethingElse -> SomethingElse
    end.

tagvalue_size(F, [H | _T]) ->
    case F#field.type of
        'DATA' ->
            {F_prev, Value_prev, _TagSize} = H,
            case F_prev#field.type of
                'LENGTH' ->
                    DataSize = erlyfix_fields:convert(Value_prev, F_prev),
                    {ok, DataSize};
                _OtherType ->
                    Err = io_lib:format("Expected that type 'LENGTH' tag should precede the"
                        ++ " current 'DATA' field type, meanwhile got field ~B ('~s') of type '~s'", [
                        F_prev#field.number, F_prev#field.name, F_prev#field.type]),
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
                                {ok, TagValue, Rest1} ->
                                    case erlyfix_fields:validate(TagValue, F) of
                                        ok    -> {ok, F, TagValue, TagNoSize + byte_size(TagValue), Rest1};
                                        error ->
                                            Str = "Value '~s' does pass validatation for field '~s'",
                                            Err = io_lib:format(Str, [TagValue, F#field.name]),
                                            Reason = erlang:iolist_to_binary(Err),
                                            {error, Reason}
                                    end;
                                not_found ->
                                    % this can actually happen if LENGTH is greater then DATA
                                    Err = io_lib:format("Canot extract tag value for field ~B (~w)", [
                                        F#field.number, F#field.name]),
                                    Reason = erlang:iolist_to_binary(Err),
                                    {error, Reason}
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
    BeginString = iolist_to_binary(io_lib:format("FIX.~B.~B", [Major, Minor])),

    case parse_tagvalue(Data, F, any_size) of
        {error, Reason} -> {error, Reason};
        not_found ->
            LengthMin = byte_size(integer_to_binary(F#field.number))
                + 1 % '=' aka separator
                + byte_size(BeginString),
            % ?DEBUG(Data),
            case byte_size(Data) >= LengthMin of
                false -> no_enough_data;
                true ->
                    Err = io_lib:format("FIX header has not been found in sequence '~w'", [Data]),
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
    case parse_tagvalue(Data, F, any_size) of
        {error, Reason} -> {error, Reason};
        not_found -> no_enough_data;
        {ok, BinaryValue, Size, Rest} ->
            % It should not throw, as it already bypassed validation
            BodyLength = erlyfix_fields:convert(BinaryValue, F),
            {ok, [{F, BodyLength, Size} | Acc], Rest}
    end.

confirm_length(Data, _Ctx, Acc) ->
    [{_F_Length, BodyLength, _TagSize} | _T] = Acc,
    case byte_size(Data) >= BodyLength of
        true -> {ok, Acc, Data};
        false -> no_enough_data
    end.

extract_checksum(Data, #context{protocol = P, data = _D}, Acc) ->
    [{_F_length, BodyLength, _F_Lengt_Size} | _T] = Acc,
    {ok, F} = erlyfix_protocol:lookup(P, {field, by_name, 'CheckSum' }),
    TagLength = iolist_size(io_lib:format(<<"~B=xxx", 1>>, [F#field.number])),
    Size = byte_size(Data),
    case Size >= BodyLength + TagLength  of
        true ->
            <<Body:BodyLength/binary, Tail/binary>> = Data,
            <<TagData:TagLength/binary, NextMessage/binary>> = Tail,
            % checksum is exactly 3 digits
            case parse_tagvalue(TagData, F, 3) of
                {error, Reason} -> {error, Reason};
                not_found ->
                    Err = io_lib:format("Checksum tag (~B) not found in sequence '~w'",
                        [F#field.number, TagData]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason};
                {ok, Value, _TagLength, _Rest} ->
                    case re:run(Value, <<"\\d{3}">>) of
                        {match,[{0,3}]} ->
                            Checksum = binary_to_integer(Value),
                            Acc1 = [
                                {F, Checksum, TagLength},
                                {next_message, NextMessage} | Acc
                            ],
                            {ok, Acc1, Body};
                        _SomethingElse ->
                            Err = io_lib:format("Checksum format mismatch: '~w' is not 3-digit string",[Value]),
                            Reason = erlang:iolist_to_binary(Err),
                            {error, Reason}
                    end
            end;
        false -> no_enough_data
    end.


confirm_checksum(Body, #context{protocol = _P, data = Data}, Acc) ->
    % need to extract all previous parsed tags to get the actual size
    % of data to be checksummed
    [{F, Checksum, ChecksumSize} | Acc0 ] = Acc,
    [_, {_F_BodyLength, BodyLength, S1}, {_F_BeginString, _, S2} | _T ] = Acc0,
    CheckSummedSize = S2 + S1 + 2 + BodyLength,
    <<SubjectData:CheckSummedSize/binary, _Tail/binary>> = Data,
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
    case parse_tagvalue(Body, F, any_size) of
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
                error:badarg ->
                    Err = io_lib:format("Unknown message type '~s'", [MsgTypeValue]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end;
        SomethingElse -> SomethingElse
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

parse_tags(<<>>, _P, Acc) -> {ok, Acc};
parse_tags(Body, P, Acc) ->
    case parse_tagpair(Body, P, Acc) of
        {ok, F, Value, TagSize, Rest} ->
            parse_tags(Rest, P, [ {F, Value, TagSize} | Acc ]);
        _OtherResult -> _OtherResult
    end.

finish_classify_scope(L, {Scope, _C4N}, MandatoryLeft, Acc, _Container) ->
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

classify_item([{F, V, _Size} | T], C, Acc0, _Container) when element(1, C) =:= field ->
    Acc1 = [{field, F#field.name, F, V} | Acc0],
    {ok, T, Acc1};
classify_item([{_F, V, _Size} | T], C, Acc0, Container) when element(1, C) =:= group ->
    Count = binary_to_integer(V),
    ScopeCTX = {C#group.name, Count},
    C4F = C#group.composite4field,
    MC = C#group.mandatoryComposites,
    % should fail, as C4F on top level all possibilitites
    {ok, Acc1, L1} = start_classify_scope(T, {group, ScopeCTX, C4F, MC}, Container),
    {ok, L1, lists:reverse(Acc1) ++ Acc0};
classify_item(L, C, Acc0, Container) when element(1, C) =:= component ->
    C4F = C#component.composite4field,
    MC = C#component.mandatoryComposites,
    ScopeCTX = {C#component.name},
    {ok, Acc1, L1} = start_classify_scope(L, {component, ScopeCTX, C4F, MC}, Container),
    {ok, L1, lists:reverse(Acc1) ++ Acc0}.

classify_scope([], Current, MandatoryLeft, Acc, Container) ->
    finish_classify_scope([], Current, MandatoryLeft, Acc, Container);
classify_scope([H | _T] = L, {_Scope, C4F} = Current, MandatoryLeft, Acc, Container) ->
    F = element(1, H),
    case maps:find(F#field.id, C4F) of
        {ok, Composite_Id} ->
            C = array:get(Composite_Id, Container),
            C_name = erlyfix_composite:name(C),
            MandatoryLeft1 = maps:remove(C_name, MandatoryLeft),
            case classify_item(L, C, Acc, Container) of
                {ok, L1, Acc1} -> classify_scope(L1, Current, MandatoryLeft1, Acc1, Container);
                _OtherResult -> _OtherResult
            end;
        error -> finish_classify_scope(L, Current, MandatoryLeft, Acc, Container)
    end.
start_classify_scope(List, {Scope, ScopeCTX, C4F, MC}, Container) ->
    classify_scope(List, {Scope, C4F}, MC, [{start, Scope, ScopeCTX}], Container).


classify([], _Candidates, Acc, _Container) ->
    Acc1 = lists:reverse(Acc),
    Acc2 = lists:flatten(Acc1),
    {ok, Acc2};
% classify([H | _T], [], _Acc) - is never called. If we have "unexpected"
% field, we stop processing current scope with the assumption that the field
% will be handled by the next scope. But as component has mandatory field
% it will fail to construct current scope; that's at least true for "trailer"
% and checksum field
classify(L, [Scope | ScopeTail], Acc, Container) ->
    case start_classify_scope(L, Scope, Container) of
        {ok, InnerAcc, L1} ->
            Acc1 = [InnerAcc | Acc],
            classify(L1, ScopeTail, Acc1, Container);
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
    classify(L, Scopes, [], P#protocol.container).

parse(Data, P) when is_binary(Data) ->
    case parse_managed_fields(Data, P) of
        {ok, Acc, RestOfBody} ->
            [Message, TagMessageType, TagChecksum, {next_message, RestData} | Acc0 ] = Acc,
            % reconstruct original tags sequence
            Acc1 = [TagMessageType | Acc0],
            case parse_tags(RestOfBody, P, Acc1) of
                {ok, Acc2} ->
                    % ?DEBUG(TagChecksum),
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
