Nonterminals
expression quantity magnitud units unit 'unit?' prefix power.

Terminals
number prefix_symbol unit_symbol 'unit_symbol?' '^' '+' '-' '*' '/'.

Rootsymbol
expression.

%% The top-level rule for a complete quantity
expression -> quantity : '$1'.

%% A quantity consists of a magnitude and units
quantity -> magnitud units : {quantity, unwrap('$1'), '$2'}.

%% Magnitude (just a number, with or without sign)
magnitud -> '+' number : unwrap('$2').
magnitud -> '-' number : add_sign('-', unwrap('$2')).
magnitud -> number : unwrap('$1').

%% A list of unit_symbol expressions (multiplication and division of units)
units -> unit : ['$1'].
units -> '*' units : '$2'.
units -> '-' units : '$2'.
units -> unit '*' units : ['$1' | '$3'].
units -> unit '/' units : ['$1' | add_sign('-', '$3')].

%% A unit_symbol expression is a combination of prefix_symbol, unit_symbol, and powers
unit -> 'unit?' power : {nil, unwrap('$1'), unwrap('$2')}.
unit -> 'unit?' unit_symbol power : {unwrap('$1'), unwrap('$2'), unwrap('$3')}.
unit -> 'unit?' 'unit?' power : {unwrap('$1'), unwrap('$2'), unwrap('$3')}.
unit -> prefix unit_symbol power : {unwrap('$1'), unwrap('$2'), unwrap('$3')}.


%% Edge case handling for 'm' (meter or milli)
'unit?' -> 'unit_symbol?' : unwrap('$1').

%% A prefix, if present, or nil if no prefix
prefix -> prefix_symbol : unwrap('$1').
prefix -> '$empty' : nil.

%% Powers (optional)
power -> '^' number : unwrap('$2').
power -> '^' '+' number : unwrap('$3').
power -> '^' '-' number : add_sign('-', unwrap('$3')).
power -> '$empty' : 1.  % Default

Erlang code.

%% unwrap helps extract values from tokens
unwrap({_, V}) -> V;
unwrap({_, _, V}) -> V;
unwrap(V) -> V.

%% Add a sign to a number or list of units
add_sign(S, N) ->
    case {S, N} of
        {'-', [{P, U, V}]} -> [{P, U, -V}];
        {'-', N} -> -N;
        {_, N} -> N
    end.

%% Resolve multiplication of two quantities
resolve_mult({_, Mag1, Units1}, {_, Mag2, Units2}) ->
    
    NewUnits = simplify_units(Units1, Units2),
    NewMag = Mag1 * Mag2,
    {NewMag, NewUnits}.

%% Resolve division of two quantities
resolve_divd({_, Mag1, Units1}, {_, Mag2, Units2}) ->
    NewUnits = simplify_units_div(Units1, Units2),
    NewMag = Mag1 / Mag2,
    {NewMag, NewUnits}.

%% Simplify unit multiplication
simplify_units(Units1, Units2) ->
    lists:foldl(
        fun(Unit1, Acc) ->
            case lists:keyfind(Unit1, 2, Units2) of
                {Prefix, Unit, Power} ->
                    [{Prefix, Unit, Power + unwrap(Unit1)} | Acc];
                false ->
                    [Unit1 | Acc]
            end
        end,
        Units2,
        Units1
    ).

%% Simplify unit division
simplify_units_div(Units1, Units2) ->
    lists:foldl(
        fun(Unit1, Acc) ->
            case lists:keyfind(Unit1, 2, Units2) of
                {Prefix, Unit, Power} ->
                    [{Prefix, Unit, Power - unwrap(Unit1)} | Acc];
                false ->
                    [Unit1 | Acc]
            end
        end,
        Units1,
        Units2
    ).
