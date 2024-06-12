-module(physerl_quantity).

-include("physerl.hrl").

-export([create_quantity/2, get_magnitude/1, add/2, subtract/2, multiply/2, divide/2]).
-export_type([quantity/0]).

%--- Types --------------------------------------------------------------------

-opaque quantity() :: {float(), unit}.

%--- API ----------------------------------------------------------------------

-spec create_quantity(float(), atom()) -> {ok, quantity()} | {error, term()}.
create_quantity(Magnitude, Unit) when is_float(Magnitude), is_atom(Unit) ->
    case physerl_unit:is_unit(Unit) of
        true -> UnitInfo = physerl_unit:unit_info(Unit),
            {ok, {Magnitude * math:pow(10, UnitInfo#unit.power_of_ten), Unit}};
        false -> {error, unknown_unit}
    end;
create_quantity(_, Unit) when is_atom(Unit) ->
    {error, invalid_magnitude};
create_quantity(Magnitude, _) when is_float(Magnitude) ->
    {error, invalid_unit};
create_quantity(_, _) ->
    {error, invalid_arguments}.

-spec get_magnitude(quantity()) -> {ok, float()} | {error, term()}.
get_magnitude({Magnitude, Unit}) when is_float(Magnitude), is_atom(Unit) ->
    case physerl_unit:is_unit(Unit) of
        true -> {ok, Magnitude};
        false -> {error, invalid_quantity}
    end;
get_magnitude(_) ->
    {error, invalid_arguments}.

-spec add(quantity(), quantity()) -> {ok, quantity()} | {error, term()}.
add({Magnitude1, Unit1}, {Magnitude2, Unit2}) when is_float(Magnitude1), is_float(Magnitude2), is_atom(Unit1), is_atom(Unit2) ->
    case {physerl_unit:is_unit(Unit1), physerl_unit:is_unit(Unit2)} of
        {true, true} ->
            case physerl_unit:compatible_units(Unit1, Unit2) of
                true -> {ok, {Magnitude1 + Magnitude2 * physerl_unit:conversion_factor(Unit2, Unit1), Unit1}};
                false -> {error, incompatible_units}
            end;
        {false, true} -> {error, invalid_quantity1};
        {true, false} -> {error, invalid_quantity2};
        _ -> {error, invalid_quantities}
    end;
add(_, _) ->
    {error, invalid_arguments}.

-spec subtract(quantity(), quantity()) -> {ok, quantity()} | {error, term()}.
subtract({Magnitude1, Unit1}, {Magnitude2, Unit2}) when is_float(Magnitude1), is_float(Magnitude2), is_atom(Unit1), is_atom(Unit2) ->
    case {physerl_unit:is_unit(Unit1), physerl_unit:is_unit(Unit2)} of
        {true, true} ->
            case physerl_unit:compatible_units(Unit1, Unit2) of
                true -> {ok, {Magnitude1 - Magnitude2 * physerl_unit:conversion_factor(Unit2, Unit1), Unit1}};
                false -> {error, incompatible_units}
            end;
        {false, true} -> {error, invalid_quantity1};
        {true, false} -> {error, invalid_quantity2};
        _ -> {error, invalid_quantities}
    end;
subtract(_, _) ->
    {error, invalid_arguments}.

-spec multiply(quantity(), float()) -> {ok, quantity()} | {error, term()}.
multiply({Magnitude, Unit}, Scalar) when is_float(Magnitude), is_atom(Unit), is_float(Scalar) ->
    case physerl_unit:is_unit(Unit) of
        true -> {ok, {Magnitude * Scalar, Unit}};
        false -> {error, invalid_quantity}
    end;
multiply(_, Scalar) when is_float(Scalar) ->
    {error, invalid_quantity};
multiply(_, _) ->
    {error, invalid_arguments}.

-spec divide(quantity(), float()) -> {ok, quantity()} | {error, term()}.
divide({Magnitude, Unit}, Scalar) when is_float(Magnitude), is_atom(Unit), is_float(Scalar) ->
    case physerl_unit:is_unit(Unit) of
        true -> {ok, {Magnitude / Scalar, Unit}};
        false -> {error, invalid_quantity}
    end;
divide(_, Scalar) when is_float(Scalar) ->
    {error, invalid_quantity};
divide(_, _) ->
    {error, invalid_arguments}.
