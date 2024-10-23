-module(physerl_si).

-export([units/0, prefixes/0]).

-export_type([unit/0]).
-export_type([base/0]).
-export_type([prefix/0]).


%--- Records ------------------------------------------------------------------

-include("physerl.hrl").

%--- Types --------------------------------------------------------------------

-type unit() :: #unit{}.
-type base() :: #{atom() => integer()}.
-type prefix() :: #prefix{}.
%--- API ----------------------------------------------------------------------

-spec units() -> [unit()].
units() ->
    [%--- Base Units -----------------------------------------------------------
     #unit{sym = m, base = #{m => 1}},
     #unit{sym = kg, base = #{kg => 1}},
     #unit{sym = s, base = #{s => 1}},
     #unit{sym = 'A', base = #{'A' => 1}},
     #unit{sym = 'K', base = #{'K' => 1}},
     #unit{sym = mol, base = #{mol => 1}},
     #unit{sym = cd, base = #{cd => 1}},
     %--- Derived Units --------------------------------------------------------
     #unit{sym = g, base = #{kg => 1}, factor = 1.0e-3},
     #unit{sym = 'Hz', base = #{s => -1}},
     #unit{sym = 'N', base = #{m => 1, kg => 1, s => -2}},
     #unit{sym = 'Pa', base = #{kg => 1, m => -1, s => -2}},
     #unit{sym = 'J', base = #{kg => 1, m => 2, s => -2}},
     #unit{sym = 'W', base = #{kg => 1, m => 2, s => -3}},
     #unit{sym = 'C', base = #{s => 1, 'A' => 1}},
     #unit{sym = 'V', base = #{kg => 1, m => 2, s => -3, 'A' => -1}},
     #unit{sym = 'F', base = #{s => 4, 'A' => 2, kg => -1, m => -2}},
     #unit{sym = 'Ohm', base = #{kg => 1, m => 2, s => -3, 'A' => -2}},
     #unit{sym = 'S', base = #{kg => -1, m => -2, s => 3, 'A' => 2}},
     #unit{sym = 'Wb', base = #{kg => 1, m => 2, s => -2, 'A' => -1}},
     #unit{sym = 'T', base = #{kg => 1, s => -2, 'A' => -1}},
     #unit{sym = 'H', base = #{kg => 1, m => 2, s => -2, 'A' => -2}},
     #unit{sym = lm, base = #{cd => 1}},
     #unit{sym = lx, base = #{cd => 1, m => -2}},
     #unit{sym = 'Bq', base = #{s => -1}},
     #unit{sym = 'Gy', base = #{m => 2, s => -2}},
     #unit{sym = 'Sv', base = #{m => 2, s => -2}},
     #unit{sym = kat, base = #{mol => 1, s => -1}}].

-spec prefixes() -> [prefix()].
prefixes() ->
    [#prefix{sym = 'Q', val = 1.0e30},
     #prefix{sym = 'R', val = 1.0e27},
     #prefix{sym = 'Y', val = 1.0e24},
     #prefix{sym = 'Z', val = 1.0e21},
     #prefix{sym = 'E', val = 1.0e18},
     #prefix{sym = 'P', val = 1.0e15},
     #prefix{sym = 'T', val = 1.0e12},
     #prefix{sym = 'G', val = 1.0e9},
     #prefix{sym = 'M', val = 1.0e6},
     #prefix{sym = 'k', val = 1.0e3},
     #prefix{sym = 'h', val = 1.0e2},
     #prefix{sym = 'da', val = 1.0e1},
     #prefix{sym = 'd', val = 1.0e-1},
     #prefix{sym = 'c', val = 1.0e-2},
     #prefix{sym = 'm', val = 1.0e-3},
     #prefix{sym = 'mu', val = 1.0e-6},
     #prefix{sym = 'n', val = 1.0e-9},
     #prefix{sym = 'p', val = 1.0e-12},
     #prefix{sym = 'f', val = 1.0e-15},
     #prefix{sym = 'a', val = 1.0e-18},
     #prefix{sym = 'z', val = 1.0e-21},
     #prefix{sym = 'y', val = 1.0e-24},
     #prefix{sym = 'r', val = 1.0e-27},
     #prefix{sym = 'q', val = 1.0e-30}].
