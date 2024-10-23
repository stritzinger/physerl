Definitions.
DIGIT    = [0-9]
FLOAT    = {DIGIT}+(\.{DIGIT}+)?([eE][+-]?{DIGIT}+)?
WS       = [\s\t]


Rules.
{DIGIT}+ : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT}  : {token, {number, TokenLine, list_to_float(TokenChars)}}.
\^       : {token, {'^', TokenLine}}.
\+       : {token, {'+', TokenLine}}.
\-       : {token, {'-', TokenLine}}.
\*       : {token, {'*', TokenLine}}.
\/       : {token, {'/', TokenLine}}.
{WS}     : {token, {' ', TokenLine}}.

%--- kg: Special Case ---------------------------------------------------------
kg       : {token, {unit_symbol, 'kg'}}.
k        : {token, {prefix_symbol, 'k'}}.
g        : {token, {unit_symbol, 'g'}}.

%--- m: Special Case ----------------------------------------------------------
mu       : {token, {prefix_symbol, 'mu'}}.
m        : {token, {'unit_symbol?', 'm'}}.

%--- Prefixes -----------------------------------------------------------------
Q        : {token, {prefix_symbol, 'Q'}}.
R        : {token, {prefix_symbol, 'R'}}.
Y        : {token, {prefix_symbol, 'Y'}}.
Z        : {token, {prefix_symbol, 'Z'}}.
E        : {token, {prefix_symbol, 'E'}}.
P        : {token, {prefix_symbol, 'P'}}.
T        : {token, {prefix_symbol, 'T'}}.
G        : {token, {prefix_symbol, 'G'}}.
M        : {token, {prefix_symbol, 'M'}}.
h        : {token, {prefix_symbol, 'h'}}.
da       : {token, {prefix_symbol, 'da'}}.
d        : {token, {prefix_symbol, 'd'}}.
c        : {token, {prefix_symbol, 'c'}}.
n        : {token, {prefix_symbol, 'n'}}.
p        : {token, {prefix_symbol, 'p'}}.
f        : {token, {prefix_symbol, 'f'}}.
a        : {token, {prefix_symbol, 'a'}}.
z        : {token, {prefix_symbol, 'z'}}.
y        : {token, {prefix_symbol, 'y'}}.
r        : {token, {prefix_symbol, 'r'}}.
q        : {token, {prefix_symbol, 'q'}}.

%--- Base Units ---------------------------------------------------------------
s        : {token, {unit_symbol, 's'}}.
A        : {token, {unit_symbol, 'A'}}.
K        : {token, {unit_symbol, 'K'}}.
mol      : {token, {unit_symbol, 'mol'}}.
cd       : {token, {unit_symbol, 'cd'}}.

%--- Derived Units ------------------------------------------------------------
Hz       : {token, {unit_symbol, 'Hz'}}.
N        : {token, {unit_symbol, 'N'}}.
Pa       : {token, {unit_symbol, 'Pa'}}.
J        : {token, {unit_symbol, 'J'}}.
W        : {token, {unit_symbol, 'W'}}.
C        : {token, {unit_symbol, 'C'}}.
V        : {token, {unit_symbol, 'V'}}.
F        : {token, {unit_symbol, 'F'}}.
Ohm      : {token, {unit_symbol, 'Ohm'}}.
S        : {token, {unit_symbol, 'S'}}.
Wb       : {token, {unit_symbol, 'Wb'}}.
T        : {token, {unit_symbol, 'T'}}.
H        : {token, {unit_symbol, 'H'}}.
lm       : {token, {unit_symbol, 'lm'}}.
lx       : {token, {unit_symbol, 'lx'}}.
Bq       : {token, {unit_symbol, 'Bq'}}.
Gy       : {token, {unit_symbol, 'Gy'}}.
Sv       : {token, {unit_symbol, 'Sv'}}.
kat      : {token, {unit_symbol, 'kat'}}.

.        : {error, {illegal_token, TokenChars}}.


Erlang code.
