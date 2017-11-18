-module(erlyfix_fields).
-include("include/erlyfix.hrl").
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

-export([serialize_field/4, validate/3, convert/2, as_label/2, compile/1]).

serialize_field({Size, Acc}, F, unchecked, Value) ->
    Value_Bits = if
        is_integer(Value) -> erlang:integer_to_list(Value);
        is_atom(Value)    -> erlang:atom_to_list(Value);
        is_list(Value)    -> Value;
        is_binary(Value)  -> erlang:binary_to_list(Value)
    end,
    Number_Bits = erlang:integer_to_list(F#field.number),
    Bits = [Number_Bits, "=", Value_Bits, 1],
    NewSize = Size + 2 + length(Number_Bits) + length(Value_Bits),
    {ok, {NewSize, [ Bits | Acc]} };
serialize_field({Size, Acc}, F, checked, RawValue) ->
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
        {ok, Value} -> serialize_field({Size, Acc}, F, unchecked, Value);
        {error, Descr} -> {error, Descr}
    end.

extract(D0, Submatches, Converter) ->
    F = fun({Skip, Count}) ->
        <<_D1:Skip/binary, D2:Count/binary, _D3/binary>> = D0,
        V = Converter(D2),
        V
    end,
    lists:map(F, Submatches).

validate_by_re(Value, Re) ->
    Size = byte_size(Value),
    case re:run(Value, Re) of
        {match,[{0, Size}]} -> ok;
        _Else              -> error
    end.

validate_field('STRING', Value, H) ->
    case re:run(Value, H#parser_helpers.tag_separator) of
        {match, _Any} -> error;
        nomatch       -> ok
    end;
validate_field('LENGTH', Value, H) -> validate_by_re(Value, H#parser_helpers.digits);
validate_field('INT', Value, H) -> validate_by_re(Value, H#parser_helpers.int);
validate_field('FLOAT', Value, H) -> validate_by_re(Value, H#parser_helpers.float);
validate_field('CHAR', Value, H) ->
    case byte_size(Value) of
        1    -> validate_field('STRING', Value, H);
        _Any -> error
    end;
validate_field('CURRENCY', Value, H) ->
    case byte_size(Value) of
        3    -> validate_field('STRING', Value, H);
        _Any -> error
    end;
validate_field('COUNTRY', Value, H) -> validate_by_re(Value, H#parser_helpers.country);
validate_field('BOOLEAN', Value, H) -> validate_by_re(Value, H#parser_helpers.boolean);
validate_field('UTCTIMEONLY', Value, Helper) ->
    Size = byte_size(Value),
    case re:run(Value, Helper#parser_helpers.utctimeonly) of
        {match, [{0, Size} | Submatches ]} ->
            [H, M, S | T] = extract(Value, Submatches, fun binary_to_integer/1),
            SS = case T of [] -> 0; [SubSeconds] -> SubSeconds end,
            R = ((H >= 0) andalso (H < 24)) andalso
                ((M >= 0) andalso (M < 60)) andalso
                ((S >= 0) andalso (S < 60)) andalso
                ((SS >= 0) andalso (SS < 1000)),
            case R of  true -> ok; false -> error end;
        _Any -> error
    end;
validate_field('MONTHYEAR', Value, H) ->
    Size = byte_size(Value),
    case re:run(Value, H#parser_helpers.monthyear) of
        {match, [{0, Size}, YY, MM | Rest ]} ->
            R = case Rest of
                [] ->
                    [Y, M] = extract(Value, [YY, MM], fun binary_to_integer/1),
                    (Y > 1) andalso (M < 13);
                [_W,_W, WW] ->
                    [Y, M, W] = extract(Value, [YY, MM, WW], fun binary_to_integer/1),
                    (Y > 1) andalso (M < 13) andalso (W > 0) andalso (W < 6);
                [DD, _S1, _S2, DD] ->
                    [Y, M, D] = extract(Value, [YY, MM, DD], fun binary_to_integer/1),
                    (Y > 1) andalso (M < 13) andalso (D < 32)
            end,
            case R of  true -> ok; false -> error end;
        _Any -> error
    end;
validate_field('LOCALMKTDATE', Value, H) ->
    Size = byte_size(Value),
    case re:run(Value,H#parser_helpers.localmktdate) of
        {match, [{0, Size} | Submatches ]} ->
            [_Y, M, D] = extract(Value, Submatches, fun binary_to_integer/1),
            R = (M < 13) andalso (D < 32),
            case R of  true -> ok; false -> error end;
        _Any -> error
    end;
validate_field('UTCTIMESTAMP', Value, H) ->
    Size = byte_size(Value),
    case re:run(Value, H#parser_helpers.utctimestamp) of
        {match, [{0, Size}, D_ref, T_ref]} ->
            [D_Bin, T_Bin] = extract(Value, [D_ref, T_ref], fun(X) -> X end),
            case validate_field('LOCALMKTDATE', D_Bin, H) of
                ok -> validate_field('UTCTIMEONLY', T_Bin, H);
                error -> error
            end;
        _Any -> error
    end;
validate_field('DATA', _Value, _H) -> ok;
% aliases
validate_field('MULTIPLEVALUESTRING', Value, H) -> validate_field('STRING', Value, H);
validate_field('EXCHANGE', Value, H) -> validate_field('STRING', Value, H);
validate_field('SEQNUM', Value, H) -> validate_field('LENGTH', Value, H);
validate_field('NUMINGROUP', Value, H) -> validate_field('LENGTH', Value, H);
validate_field('AMT', Value, H) -> validate_field('FLOAT', Value, H);
validate_field('PERCENTAGE', Value, H) -> validate_field('FLOAT', Value, H);
validate_field('PRICE', Value, H) -> validate_field('FLOAT', Value, H);
validate_field('QTY', Value, H) -> validate_field('FLOAT', Value, H);
validate_field('PRICEOFFSET', Value, H) -> validate_field('FLOAT', Value, H);
validate_field('UTCDATEONLY', Value, H) -> validate_field('LOCALMKTDATE', Value, H).


validate(Value, F, Helpers) when is_binary(Value) ->
    validate_field(F#field.type, Value, Helpers).


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
    [H, M, S | T] = extract(Value, Submatches, fun binary_to_integer/1),
    SS = case T of [] -> 0; [SubSeconds] -> SubSeconds end,
    #utc_time { hour = H, minute = M, second = S, ms = SS};
convert_field('MONTHYEAR', Value) ->
    Re = <<"(\\d{4})(\\d{2})(?|((w(\\d))|(\\d{2})))?">>,
    {match, [{0, _Size}, YY, MM | Rest]} = re:run(Value, Re),
    [Y, M] = extract(Value, [YY, MM], fun binary_to_integer/1),
    case Rest of
        [] -> #monthyear_week{ year = Y, month = M, week = 0 };
        [_W,_W, WW] ->
            [W] = extract(Value, [WW], fun binary_to_integer/1),
            #monthyear_week{ year = Y, month = M, week = W };
        [DD, _S1, _S2, DD] ->
            [D] = extract(Value, [DD], fun binary_to_integer/1),
            #monthyear_day{ year = Y, month = M, day = D }
    end;
convert_field('LOCALMKTDATE', Value) ->
    Re = <<"(\\d{4})(\\d{2})(\\d{2})">>,
    {match, [{0, _Total} | Submatches ]} = re:run(Value, Re),
    [Y, M, D] = extract(Value, Submatches, fun binary_to_integer/1),
    #fix_date { year = Y, month = M, day = D};
convert_field('UTCTIMESTAMP', Value) ->
    [V_Date, V_Time] = re:split(Value, <<"-">>, [{return, binary}]),
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

compile(Helpers) ->
    {ok, Int} = re:compile(<<"-?\\d+">>),
    {ok, Float} = re:compile(<<"-?\\d+(?:.\\d+)?">>),
    {ok, Country} = re:compile(<<"[A-Z]{2}">>),
    {ok, Boolean} = re:compile(<<"Y|N">>),
    {ok, UTCTime} = re:compile(<<"(\\d{2}):(\\d{2}):(\\d{2})(?:\\.(\\d{3}))?">>),
    {ok, Monthyear} = re:compile(<<"(\\d{4})(\\d{2})(?|((w(\\d))|(\\d{2})))?">>),
    {ok, Localmktdate} = re:compile(<<"(\\d{4})(\\d{2})(\\d{2})">>),
    {ok, Utctimestamp} = re:compile(<<"(.+)-(.+)">>),
    Helpers#parser_helpers {
        int             = Int,
        float           = Float,
        country         = Country,
        boolean         = Boolean,
        utctimeonly     = UTCTime,
        monthyear       = Monthyear,
        localmktdate    = Localmktdate,
        utctimestamp    = Utctimestamp
    }.
