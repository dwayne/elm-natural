module Main exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner as BR
import Natural


main : BR.BenchmarkProgram
main =
    BR.program benchmarks


benchmarks : Benchmark
benchmarks =
    describe "Natural"
        [ exponentiationBenchmarks
        ]


exponentiationBenchmarks : Benchmark
exponentiationBenchmarks =
    describe "exponentiation"
        [ benchmark "2 ^ 1000" <|
            \_ ->
                Natural.fromInt 1000
                    |> Maybe.map (Natural.exp Natural.two)
        --
        -- This benchmark really slows things down.
        -- , benchmark "2 ^ 10000" <|
        --     \_ ->
        --         Natural.fromInt 10000
        --             |> Maybe.map (Natural.exp Natural.two)
        ]
