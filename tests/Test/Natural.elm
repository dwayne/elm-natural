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
        , baseBStringConversionSuite
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


baseBStringConversionSuite : Test
baseBStringConversionSuite =
    describe "fromBaseBString / toBaseBString conversion"
        [ fuzz baseBString "base b string" <|
            \(b, s) ->
                Natural.fromBaseBString b s
                    |> Maybe.andThen (Natural.toBaseBString b)
                    |> Expect.equal (Just <| toCanonicalBaseBString s)
        ]


toCanonicalBaseBString : String -> String
toCanonicalBaseBString =
    removeLeadingZeros >> String.toLower


removeLeadingZeros : String -> String
removeLeadingZeros s =
    case String.uncons s of
        Just (_, "") ->
            s

        Just ('0', t) ->
            removeLeadingZeros t

        _ ->
            s


-- CUSTOM FUZZERS


negativeInt : Fuzzer Int
negativeInt =
    Fuzz.intAtMost -1


nonNegativeInt : Fuzzer Int
nonNegativeInt =
    Fuzz.intAtLeast 0


-- Generates random base b (2 <= b <= 36) strings of at least 1 character
-- and at most 100 characters.
baseBString : Fuzzer (Int, String)
baseBString =
    Fuzz.intRange 2 36
        |> Fuzz.andThen
            (\b ->
                Fuzz.listOfLengthBetween 1 100 (baseBChar b)
                    |> Fuzz.map
                        (\l ->
                            (b, String.fromList l)
                        )
            )


baseBChar : Int -> Fuzzer Char
baseBChar b =
    if 2 <= b && b <= 36 then
        Fuzz.uniformInt (b - 1)
            |> Fuzz.map
                (\offset ->
                    Char.fromCode <|
                        if offset < 10 then
                            0x30 + offset

                        else
                            (if modBy 2 offset == 0 then 0x61 else 0x41) +
                                offset - 10
                )

    else
        Fuzz.invalid "baseBChar: the base must be between 2 and 36 inclusive"
