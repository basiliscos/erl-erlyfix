-module(erlyfix_fields).
-include("erlyfix_records.hrl").

%-define(TAG_SEPARATOR, <<1:8>>).
-export([serialize_field/3]).

serialize_field({Size, Acc}, F, Value) ->
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

