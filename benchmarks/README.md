# Elm Natural Benchmarks

This project contains various benchmarks for the `elm-natural` library. It currently includes benchmarks for
**comparison**, **multiplication**, **division with remainder**, and **exponentiation**.

## Usage

**N.B.** *You need to be within the Nix development shell to run the following commands.*

### Build

Build the benchmarks.

```sh
$ build-benchmarks
```

### Run

Serve the benchmarks.

```sh
$ serve-benchmarks
```

Then, open `http://localhost:8002` in your browser to run the benchmarks.

## Notes about [cmditch/elm-bigint][elm-bigint]

### Comparison

| Operation | Benchmark | elm-bigint (runs/sec) | elm-natural (runs/sec) |
|---|---|---|---|
| multiplication | `999..9 (100 9's) * 999..9 (100 9's)` | 12,164 | 31,993 |
| division with remainder | `(999..9 (10 9's))^2 / 999..9 (10 9's)` | 3,990 | 85,668 |
| exponentiation | `2 ^ 1000` | 1,011 | 10,346 |

Source: https://github.com/dwayne/elm-natural/blob/compare-with-elm-bigint/benchmarks/BigIntBenchmarks.elm.

### [cmditch/elm-bigint][elm-bigint]'s division is too slow

The following calculation takes **more than 10 minutes**.

```elm
import BigInt
nines = List.repeat 10000 '9' |> String.fromList |> BigInt.fromIntString |> Maybe.withDefault (BigInt.fromInt 0)
product = BigInt.mul nines nines
BigInt.div product nines == nines
```

Whereas, the corresponding calculation using `elm-natural` takes **less than 1 second**.

```elm
import Natural as N
nines = List.repeat 10000 '9' |> String.fromList |> N.fromSafeString
product = N.mul nines nines
N.divBy nines product == Just nines
```

### [cmditch/elm-bigint][elm-bigint] fails for reasonably sized inputs

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

### Can [cmditch/elm-bigint][elm-bigint] compute [`5^4^3^2`](https://rosettacode.org/wiki/Arbitrary-precision_integers_%28included%29)?

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

[elm-bigint]: https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/
