module BigIntBenchmarks exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner as BR
import BigInt exposing (BigInt)


main : BR.BenchmarkProgram
main =
    BR.program benchmarks


benchmarks : Benchmark
benchmarks =
    describe "BigInt"
        [ multiplicationBenchmarks
        , divisionWithRemainderBenchmarks
        , exponentiationBenchmarks
        , comparisonBenchmarks
        ]


multiplicationBenchmarks : Benchmark
multiplicationBenchmarks =
    describe "multiplication"
        [ let
            oneHundredNines =
                nines 100
          in
          benchmark "999..9 (100 9's) * 999..9 (100 9's)" <|
            \_ ->
                BigInt.mul oneHundredNines oneHundredNines
        ]


divisionWithRemainderBenchmarks : Benchmark
divisionWithRemainderBenchmarks =
    describe "division with remainder"
        [ let
            tenNines =
                nines 10

            tenNinesSquared =
                BigInt.mul tenNines tenNines
          in
          benchmark "(999..9 (10 9's))^2 / 999..9 (10 9's)" <|
            \_ ->
                BigInt.divmod tenNinesSquared tenNines
        ]


exponentiationBenchmarks : Benchmark
exponentiationBenchmarks =
    describe "exponentiation"
        [ let
            oneThousand =
                BigInt.fromInt 1000
          in
          benchmark "2 ^ 1000" <|
            \_ ->
                BigInt.pow (BigInt.fromInt 2) oneThousand
        ]


comparisonBenchmarks : Benchmark
comparisonBenchmarks =
    describe "compare"
        [ let
            oneThousandBase26Digits =
                BigInt.pow (BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 26)) (BigInt.fromInt 999)

            a =
                BigInt.sub oneThousandBase26Digits (BigInt.fromInt 1)

            b =
                BigInt.sub oneThousandBase26Digits (BigInt.fromInt 2)
          in
          benchmark "compare (2^26)^999-1 and (2^26)^999-2" <|
            \_ ->
                BigInt.compare a b
        ]



-- HELPERS


nines : Int -> BigInt
nines n =
    --
    -- Returns a natural number consisting of max(1, n), 9's.
    --
    List.repeat (max 1 n) '9'
        |> String.fromList
        |> BigInt.fromIntString
        |> Maybe.withDefault (BigInt.fromInt 0)
