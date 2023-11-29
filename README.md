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

### Ways to create natural numbers from `Int` values

- `fromInt`
- `fromSafeInt`

### Ways to create natural numbers from `String` values

- `fromBinaryString`
- `fromOctalString`
- `fromDecimalString`
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
- `toDecimalString`
- `toHexString`
- `toString` (same as `toDecimalString`)
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

## Performance

This library tries to provide a reasonably efficient implementation of
extended-precision arithmetic over the natural numbers while being written
purely in Elm and using lists under the hood. Within that context it gives
reasonable performance but you can be the judge based on the information
provided below.

### Compared against [cmditch/elm-bigint](https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/)

| Operation | Benchmark | elm-bigint (runs/sec) | elm-natural (runs/sec) |
|---|---|---|---|
| multiplication | `999..9 (100 9's) * 999..9 (100 9's)` | 12,164 | 31,993 |
| division with remainder | `(999..9 (10 9's))^2 / 999..9 (10 9's)` | 3,990 | 85,668 |
| exponentiation | `2 ^ 1000` | 1,011 | 10,346 |

Source: https://github.com/dwayne/elm-natural/blob/compare-with-elm-bigint/benchmarks/BigIntBenchmarks.elm.

#### [cmditch/elm-bigint](https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/)'s division is too slow

```elm
import BigInt
nines = List.repeat 10000 '9' |> String.fromList |> BigInt.fromIntString |> Maybe.withDefault (BigInt.fromInt 0)
product = BigInt.mul nines nines
BigInt.div product nines == nines
```

The above calculation takes **more than 10 minutes**.

```elm
import Natural as N
nines = List.repeat 10000 '9' |> String.fromList |> N.fromSafeString
product = N.mul nines nines
N.divBy nines product == Just nines
```

Whereas, the corresponding calculation using `elm-natural` takes **less than 1 second**.

### [cmditch/elm-bigint](https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/) fails for reasonably sized inputs

**Multiplication**

```elm
import BigInt
x1 = BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 8192)
x2 = BigInt.mul x1 x1
x3 = BigInt.mul x2 x2
x4 = BigInt.mul x3 x3
x5 = BigInt.mul x4 x4
BigInt.mul x5 x5
-- RangeError: Maximum call stack size exceeded
```

**Exponentiation**

```elm
import BigInt
BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 16999983)
-- RangeError: Maximum call stack size exceeded
```

### Can [cmditch/elm-bigint](https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/) compute [`5^4^3^2`](https://rosettacode.org/wiki/Arbitrary-precision_integers_%28included%29)?

```elm
import BigInt
two = BigInt.fromInt 2
three = BigInt.fromInt 3
four = BigInt.fromInt 4
five = BigInt.fromInt 5
exp = BigInt.pow
BigInt.toString <| exp five (exp four (exp three two))
-- RangeError: Maximum call stack size exceeded
```

No, but `elm-natural` can do it in about **14 seconds**.

```elm
import Natural as N exposing (five, four, three, two, exp)
N.toString <| exp five (exp four (exp three two))
-- 62060698786608744707...92256259918212890625
```

## Resources

- Chapter 17 - Extended-Precision Arithmetic of [C Interfaces and Implementations: Techniques for Creating Reusable Software](https://archive.org/details/cinterfacesimple0000hans) helped me figure out how to design, organize and build the library from the ground up.
- [Lua Bint](https://github.com/edubart/lua-bint/tree/c73268472aa48554cf337c29b7550ce127f424a8#examples) inspired the examples.
- [Per Brinch Hansen, Multiple-Length Division Revisited: A Tour of the Minefield](https://surface.syr.edu/cgi/viewcontent.cgi?article=1162&context=eecs_techreports) helped me implement an efficient divide-and-correct algorithm for division with remainder.
