module Test.Natural exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Natural
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Natural"
        [ intConversionSuite
        ]


intConversionSuite : Test
intConversionSuite =
    describe "fromInt / toInt conversion"
        [ fuzz negativeInt "negative integers" <|
            \n ->
                Natural.fromInt n
                    |> Expect.equal Nothing
        , fuzz nonNegativeInt "non-negative integers" <|
            \n ->
                Natural.fromInt n
                    |> Maybe.map Natural.toInt
                    |> Expect.equal (Just n)
        , test "largest integer" <|
            \_ ->
                let
                    maxSafeInteger =
                        2 ^ 53 - 1
                in
                Natural.fromInt maxSafeInteger
                    |> Maybe.map Natural.toInt
                    |> Expect.equal (Just maxSafeInteger)
        ]


-- CUSTOM FUZZERS


negativeInt : Fuzzer Int
negativeInt =
    Fuzz.intAtMost -1


nonNegativeInt : Fuzzer Int
nonNegativeInt =
    Fuzz.intAtLeast 0
