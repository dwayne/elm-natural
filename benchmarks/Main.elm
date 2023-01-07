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
        [ let
            oneThousand =
                Natural.fromInt 1000
                    |> Maybe.withDefault Natural.zero
          in
          benchmark "2 ^ 1000" <|
              \_ ->
                  Natural.exp Natural.two oneThousand
        ]
