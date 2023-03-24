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
                N.mul oneHundredNines oneHundredNines
        ]


divisionWithRemainderBenchmarks : Benchmark
divisionWithRemainderBenchmarks =
    describe "division with remainder"
        [ let
            tenNines =
                nines 10

            tenNinesSquared =
                N.mul tenNines tenNines
          in
          benchmark "(999..9 (10 9's))^2 / 999..9 (10 9's)" <|
            \_ ->
                tenNinesSquared |> N.divModBy tenNines
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


comparisonBenchmarks : Benchmark
comparisonBenchmarks =
    describe "compare"
        [ let
            oneThousandBase26Digits =
                N.exp (N.exp N.two (N.fromSafeInt 26)) (N.fromSafeInt 999)

            a =
                N.sub oneThousandBase26Digits N.one

            b =
                N.sub oneThousandBase26Digits N.two
          in
          benchmark "compare (2^26)^999-1 and (2^26)^999-2" <|
            \_ ->
                N.compare a b
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
