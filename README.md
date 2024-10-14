# physerl: Units of Measurement Library

The `physerl` library is an Erlang-based tool for working with physical quantities and units of measurement. It supports operations with metric units like meters, kilograms, and seconds, including derived units like Newtons and Joules.

## Features
- **Prefix Handling**: Supports metric prefixes (`kilo`, `milli`, etc.) and applies appropriate conversion factors.
- **Unit Operations**: Supports addition, multiplication, division, and exponentiation of quantities with units.
- **Parsing**: Converts user input strings (e.g., `"1kg*m/s^2"`) into structured data.


## Example Usage

```erlang
1> physerl:quantity("1kg*m/s^2").
{ok, {1.0, #{kg => 1, m => 1, s => -2}}}

2> {ok, Q1} = physerl:quantity("1kg").
{ok,{1.0,#{kg => 1}}}

3> {ok, Q2} = physerl:quantity("2kg").
{ok,{2.0,#{kg => 1}}}

4> physerl:add(Q1, Q2).
{ok,{3.0,#{kg => 1}}}
```

## Library Components
- **Lexer**: Tokenizes input strings into numbers, unit symbols, prefixes, and operators.
- **Parser**: Analyzes token streams to generate structured quantities with magnitudes, units, prefixes, and powers.
- **Operations**: Handles arithmetic on quantities, such as adding, multiplying, and division.


## Future Enhancements
- Adding support for additional unit systems (imperial, US customary).
- Expanding error handling and improving user feedback on invalid input.
