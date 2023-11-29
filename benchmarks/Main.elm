module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner as BR
import Natural as N exposing (Natural)


main : BR.BenchmarkProgram
main =
    BR.program benchmarks


benchmarks : Benchmark
benchmarks =
    describe "Natural"
        [ comparisonBenchmarks
        , multiplicationBenchmarks
        , divisionWithRemainderBenchmarks
        , exponentiationBenchmarks
        ]


comparisonBenchmarks : Benchmark
comparisonBenchmarks =
    describe "comparison"
        [ let
            tenThousandBase26Digits =
                N.exp (N.exp N.two (N.fromSafeInt 26)) (N.fromSafeInt 9999)

            a =
                N.sub tenThousandBase26Digits N.one

            b =
                N.sub tenThousandBase26Digits N.two
          in
          benchmark "compare (2^26)^9999-1 and (2^26)^9999-2" <|
            \_ ->
                N.compare a b
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
                N.mul oneHundredNines oneHundredNines
        ]


divisionWithRemainderBenchmarks : Benchmark
divisionWithRemainderBenchmarks =
    describe "division with remainder"
        [ let
            oneHundredNines =
                nines 100

            oneHundredNinesSquared =
                N.mul oneHundredNines oneHundredNines
          in
          benchmark "(999..9 (100 9's))^2 / 999..9 (100 9's)" <|
            \_ ->
                oneHundredNinesSquared |> N.divModBy oneHundredNines
        ]


exponentiationBenchmarks : Benchmark
exponentiationBenchmarks =
    describe "exponentiation"
        [ let
            oneThousand =
                N.fromSafeInt 1000
          in
          benchmark "2 ^ 1000" <|
            \_ ->
                N.exp N.two oneThousand
        ]



-- HELPERS


nines : Int -> Natural
nines n =
    --
    -- Returns a natural number consisting of max(1, n), 9's.
    --
    List.repeat (max 1 n) '9'
        |> String.fromList
        |> N.fromSafeString
