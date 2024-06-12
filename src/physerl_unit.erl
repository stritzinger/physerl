-module(physerl_unit).

-include("physerl.hrl").

-export([unit_info/1, is_unit/1, conversion_factor/2, canonical_units/0, compatible_units/2]).

%--- API ----------------------------------------------------------------------

-define(units, [
    %--- Base Units -----------------------------------------------------------
    {m, #unit{name = meter, base_unit = meter, power_of_ten = 0}},
    {kg, #unit{name = kilogram, base_unit = kilogram, power_of_ten = 0}},
    {s, #unit{name = second, base_unit = second, power_of_ten = 0}},
    {'A', #unit{name = ampere, base_unit = ampere, power_of_ten = 0}},
    {'K', #unit{name = kelvin, base_unit = kelvin, power_of_ten = 0}},
    {mol, #unit{name = mole, base_unit = mole, power_of_ten = 0}},
    {cd, #unit{name = candela, base_unit = candela, power_of_ten = 0}},
    %--- Derived Units --------------------------------------------------------
    {'Hz', #unit{name = hertz, base_unit = second, power_of_ten = 0}},
    {'N', #unit{name = newton, base_unit = kilogram_meter_per_second_squared, power_of_ten = 0}},
    {'Pa', #unit{name = pascal, base_unit = kilogram_per_meter_per_second_squared, power_of_ten = 0}},
    {'J', #unit{name = joule, base_unit = kilogram_meter_squared_per_second_squared, power_of_ten = 0}},
    {'W', #unit{name = watt, base_unit = kilogram_meter_squared_per_second_cubed, power_of_ten = 0}},
    {'C', #unit{name = coulomb, base_unit = ampere_second, power_of_ten = 0}},
    {'V', #unit{name = volt, base_unit = kilogram_meter_squared_per_second_cubed_per_ampere, power_of_ten = 0}},
    {'F', #unit{name = farad, base_unit = second_per_ampere_squared_per_kilogram_per_meter_squared, power_of_ten = 0}},
    {'Ohm', #unit{name = ohm, base_unit = kilogram_meter_squared_per_second_cubed_per_ampere_squared, power_of_ten = 0}},
    {'S', #unit{name = siemens, base_unit = ampere_squared_second_cubed_per_kilogram_per_meter_squared, power_of_ten = 0}},
    {'Wb', #unit{name = weber, base_unit = kilogram_meter_squared_per_second_cubed_per_ampere, power_of_ten = 0}},
    {'T', #unit{name = tesla, base_unit = kilogram_per_second_squared_per_ampere, power_of_ten = 0}},
    {'H', #unit{name = henry, base_unit = kilogram_meter_squared_per_second_squared_per_ampere_squared, power_of_ten = 0}},
    {'lm', #unit{name = lumen, base_unit = candela, power_of_ten = 0}},
    {'lx', #unit{name = lux, base_unit = candela_per_meter_squared, power_of_ten = 0}},
    {'Bq', #unit{name = becquerel, base_unit = second, power_of_ten = 0}},
    {'Gy', #unit{name = gray, base_unit = meter_squared_per_second_squared, power_of_ten = 0}},
    {'Sv', #unit{name = sievert, base_unit = meter_squared_per_second_squared, power_of_ten = 0}},
    {'kat', #unit{name = katal, base_unit = mole_per_second, power_of_ten = 0}}
]).

is_unit(Unit) ->
    case lists:keyfind(Unit, 1, ?units) of
        {_, _} -> true;
        false -> false
    end.

unit_info(Unit) ->
    case lists:keyfind(Unit, 1, ?units) of
        {_, Info} -> Info;
        false -> error({unknown_unit, Unit})
    end.

conversion_factor(From, To) ->
    FromInfo = unit_info(From),
    ToInfo = unit_info(To),
    math:pow(10, FromInfo#unit.power_of_ten - ToInfo#unit.power_of_ten).

canonical_units() ->
    [U || {U, _} <- ?units].

compatible_units(Unit1, Unit2) ->
    UnitInfo1 = unit_info(Unit1),
    UnitInfo2 = unit_info(Unit2),
    UnitInfo1#unit.base_unit == UnitInfo2#unit.base_unit.
