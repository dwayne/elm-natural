module Test.Natural exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Natural exposing (Natural)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "Natural"
        [ intConversionSuite
        , constantsSuite
        , baseBStringConversionSuite
        , comparisonSuite
        , classificationSuite
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


comparisonSuite : Test
comparisonSuite =
    describe "compare"
        [ fuzz2
            nonNegativeInt
            nonNegativeInt
            "comparison as Int equals comparison as Natural" <|
            \a b ->
                let
                    x =
                        Natural.fromInt a

                    y =
                        Natural.fromInt b
                in
                Just (compare a b)
                    |> Expect.equal (Maybe.map2 Natural.compare x y)

        , fuzz natural "∀ n ∊ ℕ, n == n" <|
            \n ->
                Natural.compare n n
                    |> Expect.equal EQ

        , fuzz natural "∀ n ∊ ℕ, n < n + 1" <|
            \n ->
                Natural.compare n (Natural.add n Natural.one)
                    |> Expect.equal LT

        , fuzz natural "∀ n ∊ ℕ, n + 1 > n" <|
            \n ->
                Natural.compare (Natural.add n Natural.one) n
                    |> Expect.equal GT
        ]


classificationSuite : Test
classificationSuite =
    describe "predicates"
        [ describe "isZero"
            [ fuzz nonNegativeInt "if the Int is 0 then true else false" <|
                \i ->
                    let
                        n =
                            Natural.fromInt i
                    in
                    if i == 0 then
                        Maybe.map Natural.isZero n
                            |> Expect.equal (Just True)

                    else
                        Maybe.map Natural.isZero n
                            |> Expect.equal (Just False)
            ]
        , describe "isOne"
            [ fuzz nonNegativeInt "if the Int is 1 then true else false" <|
                \i ->
                    let
                        n =
                            Natural.fromInt i
                    in
                    if i == 1 then
                        Maybe.map Natural.isOne n
                            |> Expect.equal (Just True)

                    else
                        Maybe.map Natural.isOne n
                            |> Expect.equal (Just False)
            ]
        , describe "isNonZero"
            [ fuzz nonNegativeInt "if the Int is 0 then false else true" <|
                \i ->
                    let
                        n =
                            Natural.fromInt i
                    in
                    if i == 0 then
                        Maybe.map Natural.isNonZero n
                            |> Expect.equal (Just False)

                    else
                        Maybe.map Natural.isNonZero n
                            |> Expect.equal (Just True)
            ]
        , describe "isEven / isOdd"
            [ fuzz
                nonNegativeInt
                "if the Int is even/odd then the Natural is even/odd" <|
                \i ->
                    let
                        n =
                            Natural.fromInt i
                    in
                    if isEven i then
                        Maybe.map Natural.isEven n
                            |> Expect.equal (Just True)

                    else
                        Maybe.map Natural.isOdd n
                            |> Expect.equal (Just True)
            , fuzz
                natural
                "∀ n ∊ ℕ, n is even iff n + 1 is odd" <|
                \n ->
                    if Natural.isEven n then
                        Natural.isOdd (Natural.add n Natural.one)
                            |> Expect.equal True

                    else
                        ( Natural.isOdd n
                        , Natural.isEven (Natural.add n Natural.one)
                        )
                            |> Expect.equal (True, True)
            ]
        ]


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


natural : Fuzzer Natural
natural =
    baseBString
        |> Fuzz.andThen
            (\(b, s) ->
                case Natural.fromBaseBString b s of
                    Just n ->
                        Fuzz.constant n

                    Nothing ->
                        -- This should NEVER happen if both baseBString and
                        -- Natural.fromBaseBString are written correctly.
                        Fuzz.invalid <| "natural: an unexpected error"
            )


-- HELPERS


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


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0
