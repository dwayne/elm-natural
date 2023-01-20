# Elm Natural

An Elm library for computing with the [natural numbers](https://en.wikipedia.org/wiki/Natural_number),
ℕ = { 0, 1, 2, ... }.

## What's available?

### The natural numbers from 0 to 10

- `zero`
- `one`
- `two`
- `three`
- `four`
- `five`
- `six`
- `seven`
- `eight`
- `nine`
- `ten`

### Ways to create natural numbers from `Int` values

- `fromInt`
- `fromSafeInt`

### Ways to create natural numbers from `String` values

- `fromBinaryString`
- `fromOctalString`
- `fromHexString`
- `fromString` (supports binary, octal, hexadecimal, and decimal input formats)
- `fromSafeString`
- `fromBaseBString`

### Comparison operators

- `==`
- `compare`
- `isLessThan`
- `isLessThanOrEqual`
- `isGreaterThan`
- `isGreaterThanOrEqual`
- `max`
- `min`

### Predicates for classification

- `isZero`
- `isOne`
- `isPositive`
- `isEven`
- `isOdd`

### Arithmetic

- `add`
- `sub` (saturating subtraction)
- `mul`
- `divModBy` (Euclidean division)
- `divBy`
- `modBy`
- `exp`

### A way to convert to an `Int`

- `toInt` (use with caution)

### Ways to convert to a `String` representation

- `toBinaryString`
- `toOctalString`
- `toHexString`
- `toString` (decimal representation)
- `toBaseBString`

## Examples

As a simple example we can implement the [factorial](https://en.wikipedia.org/wiki/Factorial)
function.

```elm
import Natural as N exposing (Natural)

fact : Natural -> Natural
fact n =
    if N.isZero n then
      N.one

    else
      N.mul n <| fact (N.sub n N.one)
```

And then we can use it to compute `100!`.

```elm
N.toString (fact (N.fromSafeInt 100)) == "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"
```

Many more examples can be found in the `examples/src` directory.

- `Fibonacci.elm` - Computes up to the 1000th [Fibonacci number](https://en.wikipedia.org/wiki/Fibonacci_number).
- `Pi.elm` - Computes the first 100 digits of [π](https://en.wikipedia.org/wiki/Pi).
- `E.elm` - Computes the first 100 digits of [e](https://en.wikipedia.org/wiki/E_%28mathematical_constant%29).
- `BaseConversion.elm` - Converts decimal numbers to their binary, octal, and hexadecimal representations.

## References

- Chapter 17 - Extended Precision Arithmetic of [C Interfaces and Implementations: Techniques for Creating Reusable Software](https://archive.org/details/cinterfacesimple0000hans) helped me figure out how to design, organize and build the library from the ground up.
- [Lua Bint](https://github.com/edubart/lua-bint/tree/c73268472aa48554cf337c29b7550ce127f424a8#examples) inspired the examples.
