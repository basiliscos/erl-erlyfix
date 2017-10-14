-module(erlyfix_fields).
-include("erlyfix_records.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([serialize_field/3, serialize_field/4]).

serialize_field({Size, Acc}, F, raw, Value) ->
    Value_Bits = if
        is_integer(Value) -> erlang:integer_to_list(Value);
        is_atom(Value)    -> erlang:atom_to_list(Value);
        is_list(Value)    -> Value;
        is_binary(Value)  -> erlang:binary_to_list(Value)
    end,
    Number_Bits = erlang:integer_to_list(F#field.number),
    Bits = [Number_Bits, "=", Value_Bits, 1],
    NewSize = Size + 2 + length(Number_Bits) + length(Value_Bits),
    {ok, {NewSize, [ Bits | Acc]} }.

serialize_field({Size, Acc}, F, RawValue) ->
    R = case maps:size(F#field.value4description) of
        0    -> {ok, RawValue};
        _Any ->
            case maps:find(RawValue, F#field.value4description) of
                {ok, V} -> {ok, V#value_def.key};
                error ->
                    ?DEBUG(F#field.value4description),
                    ?DEBUG(RawValue),
                    Err = io_lib:format("Description '~s' is not available for field '~s'", [RawValue, F#field.name]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end
    end,

    case R of
        {ok, Value} -> serialize_field({Size, Acc}, F, raw, Value);
        {error, Descr} -> {error, Descr}
    end.

