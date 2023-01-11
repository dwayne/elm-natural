module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner as BR
import Natural exposing (Natural)


main : BR.BenchmarkProgram
main =
    BR.program benchmarks


benchmarks : Benchmark
benchmarks =
    describe "Natural"
        [ multiplicationBenchmarks
        , divisionWithRemainderBenchmarks
        , exponentiationBenchmarks
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
                Natural.mul oneHundredNines oneHundredNines
        ]


divisionWithRemainderBenchmarks : Benchmark
divisionWithRemainderBenchmarks =
    describe "division with remainder"
        [ let
            oneHundredNines =
                nines 100

            oneHundredNinesSquared =
                Natural.mul oneHundredNines oneHundredNines
          in
          benchmark "(999..9 (100 9's))^2 / 999..9 (100 9's)" <|
            \_ ->
                oneHundredNinesSquared |> Natural.divModBy oneHundredNines
        ]


exponentiationBenchmarks : Benchmark
exponentiationBenchmarks =
    describe "exponentiation"
        [ let
            oneThousand =
                Natural.fromInt 1000
                    |> Maybe.withDefault Natural.zero
          in
          benchmark "2 ^ 1000" <|
            \_ ->
                Natural.exp Natural.two oneThousand
        ]



-- HELPERS


nines : Int -> Natural
nines n =
    -- If n >= 1 then return a natural number consisting of n 9's.
    -- Otherwise, it returns 0.
    List.repeat n '9'
        |> String.fromList
        |> Natural.fromString
        |> Maybe.withDefault Natural.zero
