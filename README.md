
# physerl: Units of Measurement Library

The `physerl` library is an Erlang-based tool for working with physical quantities and units of measurement. It supports operations with metric units like meters, kilograms, and seconds, including derived units like Newtons and Joules. The library also provides built-in support for common physical constants, enabling accurate and flexible calculations.

## Features
- **Prefix Handling**: Supports metric prefixes (`kilo`, `milli`, etc.) and applies appropriate conversion factors.
- **Unit Operations**: Supports arithmetic operations (addition, subtraction, multiplication, division) and exponentiation of quantities with units.
- **Physical Constants**: Built-in constants like the speed of light (`c`) and gravitational constant (`G`) for quick and accurate computations.
- **Parsing**: Converts user input strings (e.g., `"1kg*m/s^2"`) into structured data.
- **Error Handling**: Uses exceptions for critical errors, ensuring robust and predictable behavior.

## Example Usage

```erlang
1> physerl:quantity("1kg*m/s^2").
{1.0, #{kg => 1, m => 1, s => -2}}

2> Q1 = physerl:quantity("10kg").
{10.0, #{kg => 1}}

3> Q2 = physerl:quantity("2kg").
{2.0, #{kg => 1}}

4> physerl:add(Q1, Q2).
{12.0, #{kg => 1}}

5> physerl:mul(Q1, 3).
{30.0, #{kg => 1}}

6> Q3 = physerl:quantity("9.81m/s^2"),
   physerl:mul(Q1, Q3).
{98.10000000000001, #{kg => 1, m => 1, s => -2}}
```

## Library Components
- **Lexer**: Tokenizes input strings into numbers, unit symbols, prefixes, and operators.
- **Parser**: Analyzes token streams to generate structured quantities with magnitudes, units, prefixes, and powers.
- **Operations**: Handles arithmetic on quantities, including adding, subtracting, multiplying, dividing, raising to a power, and taking roots.
- **Constants**: Provides easy access to predefined physical constants for calculations.

## Error Handling
- The library uses **exceptions** for critical errors (e.g., unknown units or invalid operations) to provide a clear and clean interface for the user.
- Internal helper functions manage specific cases with consistent exception throwing, ensuring that any error is immediately visible and actionable.

## Built-in Constants
The library includes a set of common physical constants for convenience:
- `c`: Speed of light in a vacuum (`299792458 m/s`)
- `G`: Gravitational constant (`6.67430e-11 m^3 kg^-1 s^-2`)
- `h`: Planck's constant (`6.62607015e-34 J·s`)
- ...and more (see the source for a complete list).

## Example with Constants

```erlang
1> Mass = physerl:quantity("10kg").
{10.0, #{kg => 1}}

2> Gravity = physerl:const(g).
{9.81, #{m => 1, s => -2}}

3> Force = physerl:mul(Mass, Gravity).
{98.10000000000001, #{kg => 1, m => 1, s => -2}}
```

## Future Enhancements
- Adding support for additional unit systems.
- Expanding the library's set of physical constants.
- Adding support for user-defined constants.
- Improving the parsing capabilities to handle more complex expressions.
- Enhancing error messages for better user feedback on invalid input.
