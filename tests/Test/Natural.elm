module Test.Natural exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Natural
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Natural"
        [ intConversionSuite
        , constantsSuite
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


constantsSuite : Test
constantsSuite =
    describe "constants"
        [ test "0" <|
            \_ ->
                Just Natural.zero
                    |> Expect.equal (Natural.fromInt 0)
        , test "1" <|
            \_ ->
                Just Natural.one
                    |> Expect.equal (Natural.fromInt 1)
        , test "2" <|
            \_ ->
                Just Natural.two
                    |> Expect.equal (Natural.fromInt 2)
        , test "3" <|
            \_ ->
                Just Natural.three
                    |> Expect.equal (Natural.fromInt 3)
        , test "4" <|
            \_ ->
                Just Natural.four
                    |> Expect.equal (Natural.fromInt 4)
        , test "5" <|
            \_ ->
                Just Natural.five
                    |> Expect.equal (Natural.fromInt 5)
        , test "6" <|
            \_ ->
                Just Natural.six
                    |> Expect.equal (Natural.fromInt 6)
        , test "7" <|
            \_ ->
                Just Natural.seven
                    |> Expect.equal (Natural.fromInt 7)
        , test "8" <|
            \_ ->
                Just Natural.eight
                    |> Expect.equal (Natural.fromInt 8)
        , test "9" <|
            \_ ->
                Just Natural.nine
                    |> Expect.equal (Natural.fromInt 9)
        , test "10" <|
            \_ ->
                Just Natural.ten
                    |> Expect.equal (Natural.fromInt 10)
        ]


-- CUSTOM FUZZERS


negativeInt : Fuzzer Int
negativeInt =
    Fuzz.intAtMost -1


nonNegativeInt : Fuzzer Int
nonNegativeInt =
    Fuzz.intAtLeast 0
