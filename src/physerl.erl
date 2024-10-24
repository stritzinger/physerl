-module(physerl).

-export([quantity/1, tokens/1, parse/1]).
-export([add/2, sub/2, mul/2, divide/2, pow/2, root/2, const/1]).

-export_type([quantity/0]).

%--- Records ------------------------------------------------------------------

-include("physerl.hrl").

%--- Types --------------------------------------------------------------------

-opaque quantity() :: {float(), physerl_si:unit()}.

%--- API ----------------------------------------------------------------------

-spec quantity(string()) -> quantity().
quantity(Str) ->
    case parse_quantity(Str) of
        {ok, {quantity, Magnitude, Units}} ->
            case convert_units(Units, Magnitude) of
                {ok, ConvertedUnits} ->
                    ConvertedUnits;
                {error, Reason} ->
                    throw({conversion_error, Reason})
            end;
        {error, Reason} ->
            throw({parsing_error, Reason})
    end.

-spec add(quantity(), quantity()) -> quantity().
add({Mag1, Units1}, {Mag2, Units2}) ->
    case units_equivalent(Units1, Units2) of
        true ->
            {Mag1 + Mag2, Units1};
        false ->
            throw({incompatible_units, Units1, Units2})
    end.

-spec sub(quantity(), quantity()) -> quantity().
sub({Mag1, Units1}, {Mag2, Units2}) ->
    case units_equivalent(Units1, Units2) of
        true ->
            {Mag1 - Mag2, Units1};
        false ->
            throw({incompatible_units, Units1, Units2})
    end.

-spec mul({float(), physerl_si:base() | float()}, quantity()) -> quantity().
mul({Mag1, Units1}, {Mag2, Units2}) ->
    {Mag1 * Mag2, combine_base_units(Units1, Units2)};
mul({Mag1, Units1}, Float) when is_float(Float) ->
    {Mag1 * Float, Units1}.

-spec divide(quantity(), quantity()) -> quantity().
divide({Mag1, Units1}, {Mag2, Units2}) when Mag2 =/= 0 ->
    {Mag1 / Mag2, combine_base_units(Units1, inverse_units(Units2))};
divide(_, {0, _}) ->
    throw({division_by_zero}).

-spec pow(quantity(), integer()) -> quantity().
pow({Magnitude, Units}, Power) ->
    NewMagnitude = math:pow(Magnitude, Power),
    NewUnits = maps:map(fun(_Key, Exponent) -> Exponent * Power end, Units),
    {NewMagnitude, NewUnits}.

-spec root(quantity(), integer()) -> quantity().
root({Magnitude, Units}, Root) when Root > 0 ->
    case lists:all(fun({_Key, Exponent}) -> Exponent rem Root =:= 0 end, maps:to_list(Units))
    of
        true ->
            NewMagnitude = math:pow(Magnitude, 1.0 / Root),
            NewUnits = maps:map(fun(_Key, Exponent) -> Exponent div Root end, Units),
            {NewMagnitude, NewUnits};
        false ->
            throw({negative_exponent, Units})
    end.

-spec const(atom()) -> quantity().
const(X) ->
    physerl_constants:get(X).

%--- Helpers ------------------------------------------------------------------

-spec convert_units([{atom() | nil, atom(), integer() | nil}], float()) ->
                       {ok, quantity()}.
convert_units(UnitList, Magnitude) ->
    foldl_combine_units(UnitList, #{}, Magnitude).

-spec foldl_combine_units([{atom() | nil, atom(), integer() | nil}],
                          physerl_si:base(),
                          float()) ->
                             {ok, quantity()}.
foldl_combine_units([], AccBase, Magnitude) ->
    {ok, {Magnitude, AccBase}};
foldl_combine_units([{Prefix, UnitSym, Power} | Rest], AccBase, Magnitude) ->
    case convert_unit({Prefix, UnitSym, Power}, Magnitude) of
        {ok, {AdjustedMagnitude, Base}} ->
            UpdatedBase = combine_base_units(AccBase, Base),
            foldl_combine_units(Rest, UpdatedBase, AdjustedMagnitude);
        {error, Reason} ->
            throw({unit_conversion_error, Reason})
    end.

-spec convert_unit({atom() | nil, atom(), integer() | nil}, float()) -> {ok, quantity()}.
convert_unit({PrefixSym, UnitSym, Power}, Magnitude) ->
    case find_unit(UnitSym) of
        {ok, #unit{base = Base, factor = Factor}} ->
            PrefixVal =
                case PrefixSym of
                    nil ->
                        1.0;
                    _ ->
                        case find_prefix(PrefixSym) of
                            {ok, #prefix{val = Val}} ->
                                Val;
                            _ ->
                                throw({unknown_prefix, PrefixSym})
                        end
                end,
            PowerToUse =
                if Power == nil ->
                       1;
                   true ->
                       Power
                end,
            AdjustedMagnitude = Magnitude * PrefixVal * math:pow(Factor, PowerToUse),
            UpdatedBase = update_unit(Base, PowerToUse),
            {ok, {AdjustedMagnitude, UpdatedBase}};
        {error, Reason} ->
            throw({unit_not_found, Reason})
    end.

-spec combine_base_units(physerl_si:base(), physerl_si:base()) -> physerl_si:base().
combine_base_units(Acc, Base) ->
    maps:merge_with(fun(_Key, Exp1, Exp2) -> Exp1 + Exp2 end, Acc, Base).

-spec update_unit(physerl_si:base(), integer()) -> physerl_si:base().
update_unit(BaseUnits, Power) ->
    maps:map(fun(_Key, Exponent) -> Exponent * Power end, BaseUnits).

-spec find_unit(atom()) -> {ok, physerl_si:unit()} | no_return().
find_unit(UnitSym) ->
    Units = physerl_si:units(),
    case lists:filter(fun(U) -> U#unit.sym =:= UnitSym end, Units) of
        [Unit] ->
            {ok, Unit};
        [] ->
            throw({unit_not_found, UnitSym})
    end.

-spec find_prefix(atom()) -> {ok, #prefix{}} | no_return().
find_prefix(PrefixSym) ->
    Prefixes = physerl_si:prefixes(),
    case lists:filter(fun(P) -> P#prefix.sym =:= PrefixSym end, Prefixes) of
        [Prefix] ->
            {ok, Prefix};
        [] ->
            throw({prefix_not_found, PrefixSym})
    end.

-spec units_equivalent(physerl_si:base(), physerl_si:base()) -> boolean().
units_equivalent(Units1, Units2) ->
    Units1 == Units2.

-spec inverse_units(physerl_si:base()) -> physerl_si:base().
inverse_units(Units) ->
    maps:map(fun(_Key, Exponent) -> -Exponent end, Units).

-spec parse_quantity(string()) -> {ok, term()} | {error, term()}.
parse_quantity(Str) ->
    Tokens = tokens(Str),
    parse(Tokens).

-spec tokens(string()) -> {ok, list()}.
tokens(Str) ->
    case physerl_lexer:string(Str) of
        {ok, Tokens, _} ->
            {ok, Tokens};
        {error, {_, physerl_lexer, {_, Reason}}, _} ->
            throw({invalid_quantity, Reason})
    end.

-spec parse({ok, list()} | {error, term()}) -> {ok, term()} | no_return().
parse({ok, Tokens}) ->
    case physerl_parser:parse(Tokens) of
        {ok, Terms} ->
            {ok, Terms};
        {error, {T, physerl_parser, _}} ->
            throw({invalid_quantity, T})
    end;
parse({error, _} = Error) ->
    throw({invalid_quantity, Error}).
