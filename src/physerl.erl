-module(physerl).

-export([create_quantity/1]).

%--- API ----------------------------------------------------------------------

create_quantity(String) ->
    physerl_unit:string_to_quantity(String).

%--- Helpers ------------------------------------------------------------------
% physerl:create_quantity("15km^2").
% physerl_unit:extract_magnitude(list_to_binary("15km^2")).
%
% > physerl_unit:extract_magnitude(list_to_binary("15km^2")).
