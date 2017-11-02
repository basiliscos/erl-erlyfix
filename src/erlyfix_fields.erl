-module(erlyfix_fields).
-include("erlyfix_records.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([serialize_field/3, serialize_field/4, convert/2]).

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

check_and_convert('STRING', Value) ->
    case re:run(Value, <<1>>) of
        nomatch -> {ok, Value};
        _Found -> error
    end;
check_and_convert('LENGTH', Value) ->
    Size = byte_size(Value),
    case re:run(Value, <<"\\d+">>) of
        {match,[{0,Size}]} -> {ok, binary_to_integer(Value)};
        nomatch -> error;
        _PatrialMatch -> error
    end.


convert(Value, F) when is_binary(Value) ->
    case check_and_convert(F#field.type, Value) of
        error ->
            Err = io_lib:format("Value '~s' does not match field '~s' validation", [Value, F#field.name]),
            Reason = erlang:iolist_to_binary(Err),
            {error, Reason};
        {ok, ConvertedValue} -> {ok, ConvertedValue}
    end.

