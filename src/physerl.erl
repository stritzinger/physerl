-module(physerl).

-export([quantity/1, tokens/1, parse/1]).
-export([add/2, sub/2, mul/2, divide/2, pow/2, root/2]).

-export_type([quantity/0]).

%--- Records ------------------------------------------------------------------

-include("physerl.hrl").

%--- Types --------------------------------------------------------------------

-opaque quantity() :: {float(), [physerl_si:unit()]}.

%--- API ----------------------------------------------------------------------

-spec quantity(string()) -> {ok, quantity()} | {error, term()}.
quantity(Str) ->
    case parse_quantity(Str) of
        {ok, {quantity, Magnitude, Units}} ->
            case convert_units(Units, Magnitude) of
                {ok, ConvertedUnits} ->
                    {ok, ConvertedUnits};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec add({float(), physerl_si:base()}, {float(), physerl_si:base()}) ->
             {ok, {float(), physerl_si:base()}} | {error, incompatible_units}.
add({Mag1, Units1}, {Mag2, Units2}) ->
    case units_equivalent(Units1, Units2) of
        true ->
            {ok, {Mag1 + Mag2, Units1}};
        false ->
            {error, incompatible_units}
    end.

-spec sub({float(), physerl_si:base()}, {float(), physerl_si:base()}) ->
             {ok, {float(), physerl_si:base()}} | {error, incompatible_units}.
sub({Mag1, Units1}, {Mag2, Units2}) ->
    case units_equivalent(Units1, Units2) of
        true ->
            {ok, {Mag1 - Mag2, Units1}};
        false ->
            {error, incompatible_units}
    end.

-spec mul({float(), physerl_si:base() | float()}, {float(), physerl_si:base()}) ->
             {ok, {float(), physerl_si:base()}}.
mul({Mag1, Units1}, {Mag2, Units2}) ->
    {ok, {Mag1 * Mag2, combine_base_units(Units1, Units2)}};
mul({Mag1, Units1}, Float) ->
    {ok, {Mag1 * Float, Units1}}.

-spec divide({float(), physerl_si:base()}, {float(), physerl_si:base()}) ->
                {ok, {float(), physerl_si:base()}}.
divide({Mag1, Units1}, {Mag2, Units2}) ->
    {ok, {Mag1 / Mag2, combine_base_units(Units1, inverse_units(Units2))}}.

-spec pow({float(), physerl_si:base()}, integer()) -> {ok, {float(), physerl_si:base()}}.
pow({Magnitude, Units}, Power) ->
    NewMagnitude = math:pow(Magnitude, Power),
    NewUnits = maps:map(fun(_Key, Exponent) -> Exponent * Power end, Units),
    {ok, {NewMagnitude, NewUnits}}.

-spec root({float(), physerl_si:base()}, integer()) ->
              {ok, {float(), physerl_si:base()}} | {error, negative_exponent}.
root({Magnitude, Units}, Root) when Root > 0 ->
    case lists:all(fun({_Key, Exponent}) -> Exponent rem Root =:= 0 end, maps:to_list(Units))
    of
        true ->
            NewMagnitude = math:pow(Magnitude, 1.0 / Root),
            NewUnits = maps:map(fun(_Key, Exponent) -> Exponent div Root end, Units),
            {ok, {NewMagnitude, NewUnits}};
        false ->
            {error, negative_exponent}
    end.

%--- Helpers ------------------------------------------------------------------
-spec convert_units([{atom() | nil, atom(), integer() | nil}], float()) ->
                       {ok, {float(), physerl_si:base()}} | {error, term()}.
convert_units(UnitList, Magnitude) ->
    foldl_combine_units(UnitList, #{}, Magnitude).

-spec foldl_combine_units([{atom() | nil, atom(), integer() | nil}],
                          physerl_si:base(),
                          float()) ->
                             {ok, {float(), physerl_si:base()}} | {error, term()}.
foldl_combine_units([], AccBase, Magnitude) ->
    {ok, {Magnitude, AccBase}};
foldl_combine_units([{Prefix, UnitSym, Power} | Rest], AccBase, Magnitude) ->
    case convert_unit({Prefix, UnitSym, Power}, Magnitude) of
        {ok, {AdjustedMagnitude, Base}} ->
            UpdatedBase = combine_base_units(AccBase, Base),
            foldl_combine_units(Rest, UpdatedBase, AdjustedMagnitude);
        Error ->
            Error
    end.

-spec convert_unit({atom() | nil, atom(), integer() | nil}, float()) ->
                      {ok, {float(), physerl_si:base()}} | {error, term()}.
convert_unit({PrefixSym, UnitSym, Power}, Magnitude) ->
    case find_unit(UnitSym) of
        {ok, #unit{base = Base, factor = Factor}} ->
            case PrefixSym of
                nil ->
                    PrefixVal = 1.0;
                _ ->
                    case find_prefix(PrefixSym) of
                        {ok, #prefix{val = Val}} ->
                            PrefixVal = Val;
                        _ ->
                            PrefixVal = 1.0
                    end
            end,
            PowerToUse =
                case Power of
                    nil ->
                        1;
                    _ ->
                        Power
                end,
            AdjustedMagnitude = Magnitude * PrefixVal * math:pow(Factor, PowerToUse),
            UpdatedBase = update_unit(Base, PowerToUse),
            {ok, {AdjustedMagnitude, UpdatedBase}};
        Error ->
            Error
    end.

-spec combine_base_units(physerl_si:base(), physerl_si:base()) -> physerl_si:base().
combine_base_units(Acc, Base) ->
    maps:merge_with(fun(_Key, Exp1, Exp2) -> Exp1 + Exp2 end, Acc, Base).

-spec update_unit(physerl_si:base(), integer()) -> physerl_si:base().
update_unit(BaseUnits, Power) ->
    maps:map(fun(_Key, Exponent) -> Exponent * Power end, BaseUnits).

-spec find_unit(atom()) -> {ok, physerl_si:unit()} | {error, not_found}.
find_unit(UnitSym) ->
    Units = physerl_si:units(),
    case lists:filter(fun(U) -> U#unit.sym =:= UnitSym end, Units) of
        [Unit] ->
            {ok, Unit};
        [] ->
            {error, not_found}
    end.

-spec find_prefix(atom()) -> {ok, #prefix{}} | {error, not_found}.
find_prefix(PrefixSym) ->
    Prefixes = physerl_si:prefixes(),
    case lists:filter(fun(P) -> P#prefix.sym =:= PrefixSym end, Prefixes) of
        [Prefix] ->
            {ok, Prefix};
        [] ->
            {error, not_found}
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

-spec tokens(string()) -> {ok, list()} | {error, term()}.
tokens(Str) ->
    case physerl_lexer:string(Str) of
        {ok, Tokens, _} ->
            {ok, Tokens};
        {error, {_, physerl_lexer, {_, Reason}}, _} ->
            {error, Reason}
    end.

-spec parse({ok, list()} | {error, term()}) ->
               {ok, term()} | {error, {syntax_error, term()}}.
parse({ok, Tokens}) ->
    case physerl_parser:parse(Tokens) of
        {ok, Terms} ->
            {ok, Terms};
        {error, {T, physerl_parser, _}} ->
            {error, {syntax_error, T}}
    end;
parse({error, _} = Error) ->
    Error.
