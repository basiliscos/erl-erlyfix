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

extract_from_binary(Binary, Submatches, Converter) ->
    F = fun({Skip, Count}) ->
        <<_S:Skip/binary, V:Count/binary, _T/binary>> = Binary,
        Converter(V)
    end,
    lists:map(F, Submatches).

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
validate_field('CURRENCY', Value, Size) ->
    case Size of
        3    -> validate_field('STRING', Value, Size);
        _Any -> error
    end;
validate_field('COUNTRY', Value, Size) -> validate_by_re(Value, Size, <<"[A-Z]{2}">>);
validate_field('BOOLEAN', Value, Size) -> validate_by_re(Value, Size, <<"Y|N">>);
validate_field('UTCTIMEONLY', Value, Size) ->
    Re = <<"(\\d{2}):(\\d{2}):(\\d{2})(?:\\.(\\d{3}))?">>,
    case re:run(Value, Re) of
        {match, [{0, Size} | Submatches ]} ->
            [H, M, S | T] = extract_from_binary(Value, Submatches, fun binary_to_integer/1),
            SS = case T of [] -> 0; [SubSeconds] -> SubSeconds end,
            R = ((H >= 0) andalso (H < 24)) andalso
                ((M >= 0) andalso (M < 60)) andalso
                ((S >= 0) andalso (S < 60)) andalso
                ((SS >= 0) andalso (SS < 1000)),
            case R of  true -> ok; false -> error end;
        _Any -> error
    end;
validate_field('MONTHYEAR', Value, Size) ->
    Re = <<"(\\d{4})(\\d{2})(?|((w(\\d))|(\\d{2})))?">>,
    case re:run(Value, Re) of
        {match, [{0, Size}, YY, MM | Rest ]} ->
            R = case Rest of
                [] ->
                    [Y, M] = extract_from_binary(Value, [YY, MM], fun binary_to_integer/1),
                    (Y > 1) andalso (M < 13);
                [_W,_W, WW] ->
                    [Y, M, W] = extract_from_binary(Value, [YY, MM, WW], fun binary_to_integer/1),
                    (Y > 1) andalso (M < 13) andalso (W > 0) andalso (W < 6);
                [DD, _S1, _S2, DD] ->
                    [Y, M, D] = extract_from_binary(Value, [YY, MM, DD], fun binary_to_integer/1),
                    (Y > 1) andalso (M < 13) andalso (D < 32)
            end,
            case R of  true -> ok; false -> error end;
        _Any -> error
    end;
validate_field('LOCALMKTDATE', Value, Size) ->
    Re = <<"(\\d{4})(\\d{2})(\\d{2})">>,
    case re:run(Value, Re) of
        {match, [{0, Size} | Submatches ]} ->
            [_Y, M, D] = extract_from_binary(Value, Submatches, fun binary_to_integer/1),
            R = (M < 13) andalso (D < 32),
            case R of  true -> ok; false -> error end;
        _Any -> error
    end;
validate_field('UTCTIMESTAMP', Value, Size) ->
    case re:run(Value, <<"(.+)-(.+)">>) of
        {match, [{0, Size}, {_D_pos, D_sz} = D_ref, {_T_pos, T_sz} = T_ref ]} ->
            [D_Bin, T_Bin] = extract_from_binary(Value, [D_ref, T_ref], fun(X) -> X end),
            case validate_field('LOCALMKTDATE', D_Bin, D_sz) of
                ok -> validate_field('UTCTIMEONLY', T_Bin, T_sz);
                error -> error
            end;
        _Any -> error
    end;
validate_field('DATA', _Value, _Size) -> ok;
% aliases
validate_field('MULTIPLEVALUESTRING', Value, Size) -> validate_field('STRING', Value, Size);
validate_field('EXCHANGE', Value, Size) -> validate_field('STRING', Value, Size);
validate_field('SEQNUM', Value, Size) -> validate_field('LENGTH', Value, Size);
validate_field('NUMINGROUP', Value, Size) -> validate_field('LENGTH', Value, Size);
validate_field('AMT', Value, Size) -> validate_field('FLOAT', Value, Size);
validate_field('PERCENTAGE', Value, Size) -> validate_field('FLOAT', Value, Size);
validate_field('PRICE', Value, Size) -> validate_field('FLOAT', Value, Size);
validate_field('QTY', Value, Size) -> validate_field('FLOAT', Value, Size);
validate_field('PRICEOFFSET', Value, Size) -> validate_field('FLOAT', Value, Size);
validate_field('UTCDATEONLY', Value, Size) -> validate_field('LOCALMKTDATE', Value, Size).


validate(Value, F) when is_binary(Value) ->
    validate_field(F#field.type, Value, byte_size(Value)).


convert_field('LENGTH', Value) -> binary_to_integer(Value);
convert_field('INT', Value) -> binary_to_integer(Value);
convert_field('FLOAT', Value) ->
    try binary_to_float(Value) of
        V -> V
    catch
        error:badarg -> binary_to_integer(Value)
    end;
convert_field('STRING', Value) -> Value;
convert_field('CURRENCY', Value) -> Value;
convert_field('BOOLEAN', Value) -> case Value of <<"Y">> -> true; <<"N">> -> false end;
convert_field('COUNTRY', Value) -> Value;
convert_field('UTCTIMEONLY', Value) ->
    Re = <<"(\\d{2}):(\\d{2}):(\\d{2})(?:\\.(\\d{3}))?">>,
    {match, [{0, _Total} | Submatches ]} = re:run(Value, Re),
    [H, M, S | T] = extract_from_binary(Value, Submatches, fun binary_to_integer/1),
    SS = case T of [] -> 0; [SubSeconds] -> SubSeconds end,
    #utc_time { hour = H, minute = M, second = S, ms = SS};
convert_field('MONTHYEAR', Value) ->
    Re = <<"(\\d{4})(\\d{2})(?|((w(\\d))|(\\d{2})))?">>,
    {match, [{0, _Size}, YY, MM | Rest]} = re:run(Value, Re),
    [Y, M] = extract_from_binary(Value, [YY, MM], fun binary_to_integer/1),
    case Rest of
        [] -> #monthyear_week{ year = Y, month = M, week = 0 };
        [_W,_W, WW] ->
            [W] = extract_from_binary(Value, [WW], fun binary_to_integer/1),
            #monthyear_week{ year = Y, month = M, week = W };
        [DD, _S1, _S2, DD] ->
            [D] = extract_from_binary(Value, [DD], fun binary_to_integer/1),
            #monthyear_day{ year = Y, month = M, day = D }
    end;
convert_field('LOCALMKTDATE', Value) ->
    Re = <<"(\\d{4})(\\d{2})(\\d{2})">>,
    {match, [{0, _Total} | Submatches ]} = re:run(Value, Re),
    [Y, M, D] = extract_from_binary(Value, Submatches, fun binary_to_integer/1),
    #fix_date { year = Y, month = M, day = D};
convert_field('UTCTIMESTAMP', Value) ->
    [V_Date, V_Time] = re:split(Value, <<"-">>),
    #utc_timestamp {
        date = convert_field('LOCALMKTDATE', V_Date),
        time = convert_field('UTCTIMEONLY', V_Time)
    };
convert_field('DATA', Value) -> Value;
% aliases
convert_field('MULTIPLEVALUESTRING', Value) -> convert_field('STRING', Value);
convert_field('EXCHANGE', Value) -> convert_field('STRING', Value);
convert_field('SEQNUM', Value) -> convert_field('LENGTH', Value);
convert_field('NUMINGROUP', Value) -> convert_field('LENGTH', Value);
convert_field('AMT', Value) -> convert_field('FLOAT', Value);
convert_field('PERCENTAGE', Value) -> convert_field('FLOAT', Value);
convert_field('PRICE', Value) -> convert_field('FLOAT', Value);
convert_field('QTY', Value) -> convert_field('FLOAT', Value);
convert_field('PRICEOFFSET', Value) -> convert_field('FLOAT', Value);
convert_field('UTCDATEONLY', Value) -> convert_field('LOCALMKTDATE', Value).

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
