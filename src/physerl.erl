-module(physerl).

-export([create_quantity/2, get_magnitude/1, add/2, subtract/2, multiply/2, divide/2, available_units/0]).
-export_type([quantity/0]).

%--- Types --------------------------------------------------------------------

-opaque quantity() :: physerl_quantity:quantity().

%--- API ----------------------------------------------------------------------

-spec create_quantity(float(), atom()) -> {ok, quantity()} | {error, term()}.
create_quantity(Magnitude, Unit) ->
    physerl_quantity:create_quantity(Magnitude, Unit).

-spec get_magnitude(quantity()) -> {ok, float()} | {error, term()}.
get_magnitude(Quantity) ->
    physerl_quantity:get_magnitude(Quantity).

-spec add(quantity(), quantity()) -> {ok, quantity()} | {error, term()}.
add(Quantity1, Quantity2) ->
    physerl_quantity:add(Quantity1, Quantity2).

-spec subtract(quantity(), quantity()) -> {ok, quantity()} | {error, term()}.
subtract(Quantity1, Quantity2) ->
    physerl_quantity:subtract(Quantity1, Quantity2).

-spec multiply(quantity(), float()) -> {ok, quantity()} | {error, term()}.
multiply(Quantity, Scalar) ->
    physerl_quantity:multiply(Quantity, Scalar).

-spec divide(quantity(), float()) -> {ok, quantity()} | {error, term()}.
divide(Quantity, Scalar) ->
    physerl_quantity:divide(Quantity, Scalar).

-spec available_units() -> [atom()].
available_units() ->
    physerl_unit:canonical_units().
