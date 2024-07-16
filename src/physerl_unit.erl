-module(physerl_unit).

-export([units/0, prefixes/0, string_to_quantity/1]).

%--- Records ------------------------------------------------------------------

-record(canonical_unit, {m = 0, kg = 0, s = 0, 'A' = 0, 'K' = 0, mol = 0, cd = 0}).
-record(unit, {symbol :: binary(), canonical_unit :: #canonical_unit{}, factor = 1.0}).
-record(prefix, {symbol :: binary(), value :: float()}).

%--- Types --------------------------------------------------------------------

-type unit() :: #unit{}.
-type canonical_unit() :: #canonical_unit{}.

%--- API ----------------------------------------------------------------------

-spec units() -> [{atom(), unit()}].
units() ->
    [%--- Base Units -----------------------------------------------------------
     {m, #unit{symbol = <<"m">>, canonical_unit = #canonical_unit{m = 1}}},
     {kg, #unit{symbol = <<"kg">>, canonical_unit = #canonical_unit{kg = 1}}},
     {s, #unit{symbol = <<"s">>, canonical_unit = #canonical_unit{s = 1}}},
     {'A', #unit{symbol = <<"A">>, canonical_unit = #canonical_unit{'A' = 1}}},
     {'K', #unit{symbol = <<"K">>, canonical_unit = #canonical_unit{'K' = 1}}},
     {mol, #unit{symbol = <<"mol">>, canonical_unit = #canonical_unit{mol = 1}}},
     {cd, #unit{symbol = <<"cd">>, canonical_unit = #canonical_unit{cd = 1}}},
     %--- Derived Units --------------------------------------------------------
     {'Hz', #unit{symbol = <<"Hz">>, canonical_unit = #canonical_unit{s = -1}}},
     {'N', #unit{symbol = <<"N">>, canonical_unit = #canonical_unit{m = 1, kg = 1, s = -2}}},
     {'Pa', #unit{symbol = <<"Pa">>, canonical_unit = #canonical_unit{kg = 1, m = -1, s = -2}}},
     {'J', #unit{symbol = <<"J">>, canonical_unit = #canonical_unit{kg = 1, m = 2, s = -2}}},
     {'W', #unit{symbol = <<"W">>, canonical_unit = #canonical_unit{kg = 1, m = 2, s = -3}}},
     {'C', #unit{symbol = <<"C">>, canonical_unit = #canonical_unit{s = 1, 'A' = 1}}},
     {'V', #unit{symbol = <<"V">>, canonical_unit = #canonical_unit{kg = 1, m = 2, s = -3, 'A' = -1}}},
     {'F', #unit{symbol = <<"F">>, canonical_unit = #canonical_unit{s = 4, 'A' = 2, kg = -1, m = -2}}},
     {'Ohm', #unit{symbol = <<"Ohm">>, canonical_unit = #canonical_unit{kg = 1,  m = 2, s = -3, 'A' = -2}}},
     {'S', #unit{symbol = <<"S">>, canonical_unit = #canonical_unit{kg = -1, m = -2, s = 3, 'A' = 2}}},
     {'Wb', #unit{symbol = <<"Wb">>, canonical_unit = #canonical_unit{kg = 1, m = 2, s = -2, 'A' = -1}}},
     {'T', #unit{symbol = <<"T">>, canonical_unit = #canonical_unit{kg = 1, s = -2, 'A' = -1}}},
     {'H', #unit{symbol = <<"H">>, canonical_unit = #canonical_unit{kg = 1, m = 2, s = -2, 'A' = -2}}},
     {lm, #unit{symbol = <<"lm">>, canonical_unit = #canonical_unit{cd = 1}}},
     {lx, #unit{symbol = <<"lx">>, canonical_unit = #canonical_unit{cd = 1, m = -2}}},
     {'Bq', #unit{symbol = <<"Bq">>, canonical_unit = #canonical_unit{s = -1}}},
     {'Gy', #unit{symbol = <<"Gy">>, canonical_unit = #canonical_unit{m = 2, s = -2}}},
     {'Sv', #unit{symbol = <<"Sv">>, canonical_unit = #canonical_unit{m = 2, s = -2}}},
     {kat, #unit{symbol = <<"kat">>, canonical_unit = #canonical_unit{mol = 1, s = -1}}}].

-spec prefixes() -> [{atom(), #prefix{}}].
prefixes() ->
    [{quetta, #prefix{symbol = <<"Q">>, value = 1.0e30}},
     {ronna, #prefix{symbol = <<"R">>, value = 1.0e27}},
     {yotta, #prefix{symbol = <<"Y">>, value = 1.0e24}},
     {zetta, #prefix{symbol = <<"Z">>, value = 1.0e21}},
     {exa, #prefix{symbol = <<"E">>, value = 1.0e18}},
     {peta, #prefix{symbol = <<"P">>, value = 1.0e15}},
     {tera, #prefix{symbol = <<"T">>, value = 1.0e12}},
     {giga, #prefix{symbol = <<"G">>, value = 1.0e9}},
     {mega, #prefix{symbol = <<"M">>, value = 1.0e6}},
     {kilo, #prefix{symbol = <<"k">>, value = 1.0e3}},
     {hecto, #prefix{symbol = <<"h">>, value = 1.0e2}},
     {deka, #prefix{symbol = <<"da">>, value = 1.0e1}},
     {deci, #prefix{symbol = <<"d">>, value = 1.0e-1}},
     {centi, #prefix{symbol = <<"c">>, value = 1.0e-2}},
     {milli, #prefix{symbol = <<"m">>, value = 1.0e-3}},
     {micro, #prefix{symbol = <<"μ">>, value = 1.0e-6}},
     {nano, #prefix{symbol = <<"n">>, value = 1.0e-9}},
     {pico, #prefix{symbol = <<"p">>, value = 1.0e-12}},
     {femto, #prefix{symbol = <<"f">>, value = 1.0e-15}},
     {atto, #prefix{symbol = <<"a">>, value = 1.0e-18}},
     {zepto, #prefix{symbol = <<"z">>, value = 1.0e-21}},
     {yocto, #prefix{symbol = <<"y">>, value = 1.0e-24}},
     {ronto, #prefix{symbol = <<"r">>, value = 1.0e-27}},
     {quecto, #prefix{symbol = <<"q">>, value = 1.0e-30}}].

string_to_quantity(String) ->
    Bin = list_to_binary(String),
    case extract_magnitude(Bin) of
        {ok, Magnitude, Rest} ->
            handle_unit_extraction(Magnitude, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

handle_unit_extraction(Magnitude, Rest) ->
    {ok, Power, RestUnit} = extract_power(Rest),
    case extract_prefix_and_unit(RestUnit) of
        {ok, Prefix, Unit} ->
            AdjustedMagnitude = adjust_magnitude(Magnitude, Unit),
            {ok, UnitRecord} = get_unit_by_symbol(Unit),
            {ok, AdjustedMagnitude, Unit, Unit0, Prefix, Power};
        {error, Reason} ->
            {error, Reason}
    end.


extract_magnitude(Bin) ->
    case re:run(Bin, "^[+-]?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?", [{capture, [0], binary}])
    of
        {match, [MagnitudeBin]} ->
            MagnitudeLen = byte_size(MagnitudeBin),
            Rest = binary:part(Bin, MagnitudeLen, byte_size(Bin) - MagnitudeLen),
            MagnitudeString = binary_to_list(MagnitudeBin),
            case re:run(MagnitudeString, "\\.|[eE]") of
                {match, _} ->
                    {ok, list_to_float(MagnitudeString), Rest};
                nomatch ->
                    MagnitudeWithPoint = MagnitudeString ++ ".0",
                    {ok, list_to_float(MagnitudeWithPoint), Rest}
            end;
        nomatch ->
            {error, invalid_magnitud}
    end.

extract_power(UnitBin) ->
    case re:run(UnitBin, "\\^([0-9]+)$", [{capture, [1], binary}]) of
        {match, [PowerBin]} ->
            PowerLen = byte_size(PowerBin) + 1,
            Rest = binary:part(UnitBin, 0, byte_size(UnitBin) - PowerLen),
            {ok, binary_to_integer(PowerBin), Rest};
        nomatch ->
            {ok, 1, UnitBin}
    end.

extract_prefix_and_unit(UnitBin) ->
    Prefixes = lists:map(fun({_, Prefix}) -> Prefix#prefix.symbol end, prefixes()),
    Units = lists:map(fun({_, Unit}) -> Unit#unit.symbol end, units()),
    PossibleCombinations = [{Prefix, Unit} || Prefix <- Prefixes, Unit <- Units],
    case lists:filter(fun({Prefix, Unit}) ->
                            binary:match(UnitBin, <<Prefix/binary, Unit/binary>>) =/= nomatch
                        end,
                        PossibleCombinations)
    of
        [{Prefix, Unit} | _] ->
            {ok, Prefix, Unit};
        [] ->
            case lists:filter(fun(Unit) -> binary:match(UnitBin, Unit) =/= nomatch end, Units) of
                [MatchUnit | _] ->
                    {ok, <<>>, MatchUnit};
                [] ->
                    {error, invalid_unit}
            end
    end.

adjust_magnitude(Magnitude, Unit) ->
    case lists:keyfind(Unit, 1, units()) of
        {_, #unit{factor = Factor}} ->
            Magnitude * Factor;
        _ ->
            Magnitude
    end.

-spec get_unit_by_symbol(binary()) -> {ok, unit()} | {error, invalid_unit}.
get_unit_by_symbol(Symbol) ->
    Units = units(),
    case lists:filter(fun({_, #unit{symbol = Sym}}) -> Sym =:= Symbol end, Units) of
        [Unit] -> {ok, Unit};
        [] -> {error, invalid_unit}
    end.