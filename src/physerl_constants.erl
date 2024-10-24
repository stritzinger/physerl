-module(physerl_constants).

-export([get/1]).

-spec get(atom()) -> physerl:quantity().
get(c) ->
    %% Speed of light in a vacuum: 299792458 m/s
    physerl:quantity("299792458*m/s");
get(g) ->
    %% Gravitational acceleration constant: 9.81 m/s^2
    physerl:quantity("9.81*m/s^2");
get('G') ->
    %% Gravitational constant: 6.67430e-11 m^3 kg^-1 s^-2
    physerl:quantity("6.67430e-11*m^3*kg^-1*s^-2");
get(h) ->
    %% Planck's constant: 6.62607015e-34 J s
    physerl:quantity("6.62607015e-34*J*s");
get(e) ->
    %% Elementary charge: 1.602176634e-19 C
    physerl:quantity("1.602176634e-19*C");
get(k_B) ->
    %% Boltzmann constant: 1.380649e-23 J/K
    physerl:quantity("1.380649e-23*J/K");
get('N_A') ->
    %% Avogadro's number: 6.02214076e23 mol^-1
    physerl:quantity("6.02214076e23*mol^-1");
get('R') ->
    %% Gas constant: 8.314462618 J mol^-1 K^-1
    physerl:quantity("8.314462618*J*mol^-1*K^-1");
get(eps0) ->
    %% Permittivity of free space: 8.8541878128e-12 F/m
    physerl:quantity("8.8541878128e-12*F/m");
get(mu0) ->
    %% Permeability of free space: 1.25663706212e-6 N/A^2
    physerl:quantity("1.25663706212e-6*N*A^-2");
get(ke) ->
    %% Coulomb's constant: 8.9875517873681764e9 N m^2/C^2
    physerl:quantity("8.9875517873681764e9*N*m^2*C^-2");
get(m_u) ->
    %% Atomic mass constant: 1.66053906660e-27 kg
    physerl:quantity("1.66053906660e-27*kg");
get(m_e) ->
    %% Electron mass: 9.10938356e-31 kg
    physerl:quantity("9.10938356e-31*kg");
get(m_p) ->
    %% Proton mass: 1.67262192369e-27 kg
    physerl:quantity("1.67262192369e-27*kg");
get(m_n) ->
    %% Neutron mass: 1.67492749804e-27 kg
    physerl:quantity("1.67492749804e-27*kg");
get(alpha) ->
    %% Fine-structure constant: 7.2973525693e-3 (dimensionless)
    physerl:quantity("7.2973525693e-3");
get(sigma) ->
    %% Stefan–Boltzmann constant: 5.670374419e-8 W m^-2 K^-4
    physerl:quantity("5.670374419e-8*W*m^-2*K^-4");
get(b) ->
    %% Wien's displacement constant: 2.897771955e-3 m K
    physerl:quantity("2.897771955e-3*m*K");
get('R_inf') ->
    %% Rydberg constant: 10973731.568160 m^-1
    physerl:quantity("10973731.568160*m^-1");
get(l_p) ->
    %% Planck length: 1.616255e-35 m
    physerl:quantity("1.616255e-35*m");
get(t_p) ->
    %% Planck time: 5.391247e-44 s
    physerl:quantity("5.391247e-44*s");
get(Sym) ->
    throw({unknown_constant, Sym}).
