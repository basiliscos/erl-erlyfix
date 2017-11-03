-module(erlyfix_fields).
-include("erlyfix_records.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([serialize_field/3, serialize_field/4, validate/2, convert/2, as_label/2]).

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

validate_by_re(Value, Size, Re) ->
    case re:run(Value, Re) of
        {match,[{0,Size}]} -> ok;
        _Else              -> error
    end.

validate_field('STRING', Value, _Size) ->
    case re:run(Value, <<1>>) of
        {match, _Any} -> error;
        nomatch       -> ok
    end;
validate_field('LENGTH', Value, Size) -> validate_by_re(Value, Size, <<"\\d+">>);
validate_field('INT', Value, Size) -> validate_by_re(Value, Size, <<"-?\\d+">>);
validate_field('FLOAT', Value, Size) -> validate_by_re(Value, Size, <<"-?\\d+(?:.\\d+)?">>);
validate_field('CHAR', Value, Size) ->
    case Size of
        1    -> validate_field('STRING', Value, Size);
        _Any -> error
    end;
validate_field('DATA', _Value, _Size) -> ok.


validate(Value, F) when is_binary(Value) ->
    validate_field(F#field.type, Value, byte_size(Value)).


convert_field('LENGTH', Value) -> binary_to_integer(Value);
convert_field('INT', Value) -> binary_to_integer(Value);
convert_field('FLOAT', Value) -> binary_to_float(Value);
convert_field('STRING', Value) -> Value;
convert_field('DATA', Value) -> Value.

convert(Value, F) -> convert_field(F#field.type, Value).

as_label(Value, F) when is_binary(Value) ->
    try binary_to_existing_atom(Value, latin1) of
        AtomValue ->
            case maps:find(AtomValue, F#field.value4key) of
                {ok, V} -> V#value_def.description;
                error -> not_found
            end
    catch
        error:badarg -> not_found
    end.



