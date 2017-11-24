-module(erlyfix_utils).
-include("erlyfix.hrl").

-export([checksum/1, lookup_field/2, lookup_field_by_no/2, lookup_message/2, lookup/2]).

checksum(Acc, []) -> Acc rem 256;
checksum(Acc, [H | T]) when is_list(H) ->
    checksum(checksum(Acc, H), T);
checksum(Acc, [H | T]) when is_integer(H) ->
    checksum(Acc + H, T).

checksum(IOList) when is_list(IOList) ->
    checksum(0, IOList);
checksum(Data) when is_binary(Data) ->
    lists:sum([Char || <<Char>> <= Data]) rem 256.

lookup_field(Name, P) ->
    {F4N, C} = case element(1, P) of
        uncompiled_protocol -> {P#uncompiled_protocol.field4name, P#uncompiled_protocol.container};
        protocol ->  {P#protocol.field4name, P#protocol.container}
    end,
    case maps:find(Name, F4N) of
        {ok, FieldId} -> {ok, array:get(FieldId, C)};
        error -> not_found
    end.

lookup_field_by_no(N, P) ->
    F4N = P#protocol.field4number,
    case maps:find(N, F4N) of
        {ok, FieldId} ->
            {ok, array:get(FieldId, P#protocol.container)};
        error -> not_found
    end.

lookup_message(Type, P) ->
    M4T = P#protocol.message4type,
    case maps:find(Type, M4T) of
        {ok, Message} -> {ok, Message};
        error -> not_found
    end.

lookup(Protocol, Criterium) ->
    {K, L, JustId} = case Criterium of
        {field, by_name, X} -> {X, Protocol#protocol.field4name, true};
        {field, by_number, X} -> {X, Protocol#protocol.field4number, true};
        {component, X} -> {X, Protocol#protocol.component4name, true};
        {message, by_name, X} -> {X, Protocol#protocol.message4name, false};
        {message, by_type, X} -> {X, Protocol#protocol.message4type, false}
    end,
    case maps:find(K, L) of
        {ok, CompositeId} ->
            %?DEBUG(CompositeId),
            case JustId of
                true ->
                    Composite = array:get(CompositeId, Protocol#protocol.container),
                    {ok, Composite};
                false ->
                    {ok, CompositeId}
            end;
        error -> not_found
    end.
