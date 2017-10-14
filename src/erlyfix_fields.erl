-module(erlyfix_fields).
-include("erlyfix_records.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([serialize_field/3, serialize_field/4]).

serialize_field({Size, Acc}, F, raw, Value) ->
    Value_Bits = if
        is_integer(Value) -> erlang:integer_to_binary(Value);
        is_atom(Value)    -> erlang:atom_to_binary(Value, latin1);
        is_list(Value)    -> erlang:list_to_binary(Value);
        is_binary(Value)  -> Value
    end,
    Number_Bits = erlang:integer_to_binary(F#field.number),
    Bits = [Number_Bits, <<"=">>, Value_Bits, <<1:8>>],
    NewSize = Size + 2 + size(Number_Bits) + size(Value_Bits),
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
                    Err = iolib:format("Description '~s' is not available for field '~s'", [RawValue, F#field.name]),
                    Reason = erlang:iolist_to_binary(Err),
                    {error, Reason}
            end
    end,

    case R of
        {ok, Value} -> serialize_field({Size, Acc}, F, raw, Value);
        {error, Descr} -> {error, Descr}
    end.

