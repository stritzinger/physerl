-module(physerl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([create_quantity_test/1, get_magnitude_test/1, add_test/1, subtract_test/1, multiply_test/1, divide_test/1]).
-compile([export_all, nowarn_export_all]).

%--- API ----------------------------------------------------------------------

all() -> [create_quantity_test, get_magnitude_test, add_test, subtract_test, multiply_test, divide_test].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

%--- Test ---------------------------------------------------------------------

create_quantity_test(_Config) ->
    % Valid input
    {ok, Quantity} = physerl:create_quantity(1.0, m),
    ?assertEqual({1.0, m}, Quantity),

    % Invalid Magnitude (integer)
    ResultInt = physerl:create_quantity(1, m),
    ?assertEqual({error, invalid_magnitude}, ResultInt),

    % Invalid Magnitude (not a number)
    ResultString = physerl:create_quantity("not_a_number", m),
    ?assertEqual({error, invalid_magnitude}, ResultString),

    % Invalid Magnitude (character code: $A)
    ResultChar = physerl:create_quantity($A, m),
    ?assertEqual({error, invalid_magnitude}, ResultChar),

    % Invalid Unit (not an atom)
    ResultNotAtom = physerl:create_quantity(1.0, "not_an_atom"),
    ?assertEqual({error, invalid_unit}, ResultNotAtom),

    % Unknown Unit
    ResultUnknownUnit = physerl:create_quantity(1.0, undefined_unit),
    ?assertEqual({error, unknown_unit}, ResultUnknownUnit).

get_magnitude_test(_Config) ->
    % Valid quantity
    {ok, Magnitude} = physerl:get_magnitude({1.0, m}),
    ?assertEqual(1.0, Magnitude),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct0 = physerl:get_magnitude({1.0, bar}),
    ?assertEqual({error, invalid_quantity}, ResultInvalidStruct0),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct1 = physerl:get_magnitude({1,0}),
    ?assertEqual({error, invalid_arguments}, ResultInvalidStruct1),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct2 = physerl:get_magnitude({1.0, "not_a_unit"}),
    ?assertEqual({error, invalid_arguments}, ResultInvalidStruct2),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct3 = physerl:get_magnitude("not_a_tuple"),
    ?assertEqual({error, invalid_arguments}, ResultInvalidStruct3).

add_test(_Config) ->
    % Valid input
    {ok, Result} = physerl:add({1.0, m}, {100.0, m}),
    ?assertEqual({101.0, m}, Result),

    % Incompatible units
    ResultIncompatible = physerl:add({1.0, m}, {1.0, kg}),
    ?assertEqual({error, incompatible_units}, ResultIncompatible),

    % Invalid quantity in first parameter
    ResultInvalidQuantity1 = physerl:add({1.0, bar}, {1.0, m}),
    ?assertEqual({error, invalid_quantity1}, ResultInvalidQuantity1),

    % Invalid quantity in second parameter
    ResultInvalidQuantity2 = physerl:add({1.0, m}, {1.0, bar}),
    ?assertEqual({error, invalid_quantity2}, ResultInvalidQuantity2),

    % Both quantities invalid
    ResultInvalidQuantities = physerl:add({1.0, bar}, {1.0, foo}),
    ?assertEqual({error, invalid_quantities}, ResultInvalidQuantities),

    % Invalid quantity structure
    ResultInvalidArguments0 = physerl:add({1, m}, {1, m}),
    ?assertEqual({error, invalid_arguments}, ResultInvalidArguments0),

    % Invalid quantity structure
    ResultInvalidArguments1 = physerl:add({1.0}, {1.0, m}),
    ?assertEqual({error,invalid_arguments}, ResultInvalidArguments1),

    % Invalid quantity structure
    ResultInvalidArguments2 = physerl:add({1.0}, {1.0}),
    ?assertEqual({error,invalid_arguments}, ResultInvalidArguments2),

    % Invalid quantity structure
    ResultInvalidArguments3 = physerl:add({1.0, "m"}, {1.0, "m"}),
    ?assertEqual({error,invalid_arguments}, ResultInvalidArguments3).

subtract_test(_Config) ->
    % Valid input
    {ok, Result} = physerl:subtract({1.0, m}, {100.0, m}),
    ?assertEqual({-99.0, m}, Result),

    % Incompatible units
    ResultIncompatible = physerl:subtract({1.0, m}, {1.0, kg}),
    ?assertEqual({error, incompatible_units}, ResultIncompatible),

    % Invalid quantity in first parameter
    ResultInvalidQuantity1 = physerl:subtract({1.0, bar}, {1.0, m}),
    ?assertEqual({error, invalid_quantity1}, ResultInvalidQuantity1),

    % Invalid quantity in second parameter
    ResultInvalidQuantity2 = physerl:subtract({1.0, m}, {1.0, bar}),
    ?assertEqual({error, invalid_quantity2}, ResultInvalidQuantity2),

    % Both quantities invalid
    ResultInvalidQuantities = physerl:subtract({1.0, bar}, {1.0, foo}),
    ?assertEqual({error, invalid_quantities}, ResultInvalidQuantities),

     % Invalid quantity structure
     ResultInvalidArguments1 = physerl:subtract({1.0}, {1.0, m}),
    ?assertEqual({error,invalid_arguments}, ResultInvalidArguments1),

    % Invalid quantity structure
    ResultInvalidArguments2 = physerl:subtract({1.0}, {1.0}),
    ?assertEqual({error,invalid_arguments}, ResultInvalidArguments2),

    % Invalid quantity structure
    ResultInvalidArguments3 = physerl:subtract({1.0, "m"}, {1.0, "m"}),
    ?assertEqual({error,invalid_arguments}, ResultInvalidArguments3).

multiply_test(_Config) ->
    % Valid input
    {ok, Result} = physerl:multiply({1.0, m}, 2.0),
    ?assertEqual({2.0, m}, Result),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct0 = physerl:multiply({1.0, bar}, 2.0),
    ?assertEqual({error, invalid_quantity}, ResultInvalidStruct0),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct1 = physerl:multiply({1.0}, 2.0),
    ?assertEqual({error, invalid_quantity}, ResultInvalidStruct1),

    % Invalid scalar (not a float)
    ResultInvalidScalar0 = physerl:multiply({1.0, m}, "2.0"),
    ?assertEqual({error, invalid_arguments}, ResultInvalidScalar0),

    % Invalid scalar (not a float)
    ResultInvalidScalar1 = physerl:multiply({1.0, m}, $A),
    ?assertEqual({error, invalid_arguments}, ResultInvalidScalar1),

    % Invalid quantity and scalar
    ResultInvalidBoth = physerl:multiply({1.0}, "2.0"),
    ?assertEqual({error, invalid_arguments}, ResultInvalidBoth).

divide_test(_Config) ->
    % Valid input
    {ok, Result} = physerl:divide({1.0, m}, 2.0),
    ?assertEqual({0.5, m}, Result),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct0 = physerl:divide({1.0, bar}, 2.0),
    ?assertEqual({error, invalid_quantity}, ResultInvalidStruct0),

    % Invalid quantity (wrong structure)
    ResultInvalidStruct1 = physerl:divide({1.0}, 2.0),
    ?assertEqual({error, invalid_quantity}, ResultInvalidStruct1),

    % Invalid scalar (not a float)
    ResultInvalidScalar0 = physerl:divide({1.0, m}, "2.0"),
    ?assertEqual({error, invalid_arguments}, ResultInvalidScalar0),

    % Invalid scalar (not a float)
    ResultInvalidScalar1 = physerl:divide({1.0, m}, $A),
    ?assertEqual({error, invalid_arguments}, ResultInvalidScalar1),

    % Invalid quantity and scalar
    ResultInvalidBoth = physerl:divide({1.0}, "2.0"),
    ?assertEqual({error, invalid_arguments}, ResultInvalidBoth).