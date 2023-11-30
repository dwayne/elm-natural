# Elm Natural

A pure Elm library for computing with the [natural numbers](https://en.wikipedia.org/wiki/Natural_number),
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

### Ways to create natural numbers from an `Int`

- `fromSafeInt`
- `fromInt`

### Ways to create natural numbers from a `String`

- `fromSafeString`
- `fromString` (supports binary, octal, decimal, and hexadecimal inputs)
- `fromBinaryString`
- `fromOctalString`
- `fromDecimalString`
- `fromHexString`
- `fromBaseBString`

### Comparison operators

- `==`
- `/=`
- `compare`
- `isLessThan`
- `isLessThanOrEqual`
- `isGreaterThan`
- `isGreaterThanOrEqual`
- `max`
- `min`

### Predicates for classification

- `isZero` (i.e. `== 0`)
- `isOne` (i.e. `== 1`)
- `isPositive` (i.e. `> 0`)
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

- `toInt` (use with caution since it discards information)

### Ways to convert to a `String`

- `toString` (same as `toDecimalString`)
- `toBinaryString`
- `toOctalString`
- `toDecimalString`
- `toHexString`
- `toBaseBString`

## Examples

### Factorial

The [factorial](https://en.wikipedia.org/wiki/Factorial) of a natural number `n`, denoted by `n!`, is the product of all
positive natural numbers less than or equal to `n`. The value of `0!` is `1` by convention.

```elm
import Natural as N exposing (Natural)

fact : Natural -> Natural
fact n =
    if N.isZero n then
      N.one

    else
      N.mul n <| fact (N.sub n N.one)
```

#### What's `100!`?

```elm
N.toString (fact (N.fromSafeInt 100)) == "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000"
```

### More examples

A few more examples can be found in the `examples/src` directory.

- `Fibonacci.elm` - Computes up to the 1000th [Fibonacci number](https://en.wikipedia.org/wiki/Fibonacci_number).
- `Pi.elm` - Computes the first 100 digits of [`π`](https://en.wikipedia.org/wiki/Pi).
- `E.elm` - Computes the first 100 digits of [`e`](https://en.wikipedia.org/wiki/E_%28mathematical_constant%29).
- `BaseConversion.elm` - Converts decimal numbers to their binary, octal, and hexadecimal representations.

## Performance

This library tries to provide a reasonably efficient implementation of extended-precision
arithmetic over the natural numbers while being written purely in Elm and making extensive
use of lists. Within that context it gives reasonable performance.

```txt
Natural / comparison

  compare (2^26)^9999-1 and (2^26)^9999-2
  runs/second     = 5,839
  goodness of fit = 99.86%

Natural / multiplication

  999..9 (100 9's) * 999..9 (100 9's)
  runs/second     = 53,038
  goodness of fit = 99.79%

Natural / division with remainder

  (999..9 (100 9's))^2 / 999..9 (100 9's)
  runs/second     = 6,852
  goodness of fit = 99.91%

Natural / exponentiation

  2 ^ 1000
  runs/second     = 15,623
  goodness of fit = 99.7%
```

**N.B.** *You can read `benchmarks/README.md` to learn how to reproduce the above benchmark report and
to see how this library compares against [`cmditch/elm-bigint`](https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/).*

## Resources

- Chapter 17 - Extended-Precision Arithmetic of [C Interfaces and Implementations: Techniques for Creating Reusable Software](https://archive.org/details/cinterfacesimple0000hans) helped me to design, organize and implement the library.
- [Lua Bint](https://github.com/edubart/lua-bint/tree/c73268472aa48554cf337c29b7550ce127f424a8#examples) inspired the examples.
- [Per Brinch Hansen, Multiple-Length Division Revisited: A Tour of the Minefield](https://surface.syr.edu/cgi/viewcontent.cgi?article=1162&context=eecs_techreports) helped me implement an efficient divide-and-correct algorithm for division with remainder.
