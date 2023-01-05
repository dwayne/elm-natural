module Test.Natural exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Natural exposing (Natural)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Natural"
        [ intConversionSuite
        , constantsSuite
        , baseBStringConversionSuite
        , comparisonSuite
        , classificationSuite
        , additionSuite
        , subtractionSuite
        , multiplicationSuite
        , divisionWithRemainderSuite
        , divisionSuite
        , exponentiationSuite
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


additionSuite : Test
additionSuite =
    describe "addition"
        [ fuzz natural "0 is the identity" <|
            \n ->
                -- n + 0 = n = 0 + n
                ( Natural.add n Natural.zero
                , Natural.add Natural.zero n
                )
                    |> Expect.equal (n, n)
        , fuzz2 natural natural "is commutative" <|
            \a b ->
                -- a + b = b + a
                Natural.add a b
                    |> Expect.equal (Natural.add b a)
        , fuzz3 natural natural natural "is associative" <|
            \a b c ->
                -- (a + b) + c = a + (b + c)
                Natural.add (Natural.add a b) c
                    |> Expect.equal (Natural.add a (Natural.add b c))
        ]


subtractionSuite : Test
subtractionSuite =
    describe "(saturating) subtraction"
        [ fuzz natural "∀ n ∊ ℕ, n - 0 = n" <|
            \n ->
                Natural.sub n Natural.zero
                    |> Expect.equal n
        , fuzz natural "∀ n ∊ ℕ, 0 - n = 0" <|
            \n ->
                Natural.sub Natural.zero n
                    |> Expect.equal Natural.zero
        , fuzz natural "∀ n ∊ ℕ, n - n = 0" <|
            \n ->
                Natural.sub n n
                    |> Expect.equal Natural.zero
        , fuzz2 natural natural "the definition" <|
            \a b ->
                let
                    c =
                        Natural.sub a b
                in
                if a |> Natural.isGreaterThanOrEqual b then
                    -- c + b = a
                    Natural.add c b
                        |> Expect.equal a

                else
                    -- c = 0
                    c |> Expect.equal Natural.zero
        ]


multiplicationSuite : Test
multiplicationSuite =
    describe "multiplication"
        [ fuzz natural "1 is the identity" <|
            \n ->
                -- n * 1 = n = 1 * n
                ( Natural.mul n Natural.one
                , Natural.mul Natural.one n
                )
                    |> Expect.equal (n, n)
        , fuzz natural "∀ n ∊ ℕ, n * 0 = 0 = 0 * n" <|
            \n ->
                ( Natural.mul n Natural.zero
                , Natural.mul Natural.zero n
                )
                    |> Expect.equal (Natural.zero, Natural.zero)
        , fuzz2 natural natural "is commutative" <|
            \a b ->
                -- a * b = b * a
                Natural.mul a b
                    |> Expect.equal (Natural.mul b a)
        , fuzz3 natural natural natural "is associative" <|
            \a b c ->
                -- (a * b) * c = a * (b * c)
                Natural.mul (Natural.mul a b) c
                    |> Expect.equal (Natural.mul a (Natural.mul b c))
        , fuzz3 natural natural natural "left-distributive over addition" <|
            \a b c ->
                -- a * (b + c) = a * b + a * c
                Natural.mul a (Natural.add b c)
                    |> Expect.equal
                        (Natural.add (Natural.mul a b) (Natural.mul a c))
        , fuzz3 natural natural natural "right-distributive over addition" <|
            \a b c ->
                -- (b + c) * a = b * a + c * a
                Natural.mul (Natural.add b c) a
                    |> Expect.equal
                        (Natural.add (Natural.mul b a) (Natural.mul c a))
        ]


divisionWithRemainderSuite : Test
divisionWithRemainderSuite =
    describe "division with remainder"
        [ fuzz2 natural natural "the definition" <|
            \a b ->
                case a |> Natural.divModBy b of
                    Just (q, r) ->
                        if a |> Natural.isLessThan b then
                            (q, r)
                                |> Expect.equal (Natural.zero, a)

                        else if a == b then
                            (q, r)
                                |> Expect.equal (Natural.one, Natural.zero)

                        else
                            -- q * b + r = a
                            Natural.add (Natural.mul q b) r
                                |> Expect.equal a

                    Nothing ->
                        b |> Expect.equal Natural.zero
        , fuzz natural "by 0 is undefined" <|
            \n ->
                n
                    |> Natural.divModBy Natural.zero
                    |> Expect.equal Nothing
        , fuzz natural "by 1" <|
            \n ->
                n
                    |> Natural.divModBy Natural.one
                    |> Expect.equal (Just (n, Natural.zero))
        , fuzz natural "by 2 and isEven / isOdd relation" <|
            \n ->
                case n |> Natural.divModBy Natural.two of
                    Just (_, r) ->
                        if r == Natural.zero then
                            Natural.isEven n
                                |> Expect.equal True

                        else
                            (r, Natural.isOdd n)
                                |> Expect.equal (Natural.one, True)

                    Nothing ->
                        Expect.fail "division by 2 is NEVER undefined"
        ]


divisionSuite : Test
divisionSuite =
    describe "division"
        [ fuzz3
            positiveNatural
            natural
            positiveNatural
            "cancel common factor" <|
            \a b c ->
                -- (a * b) / (a * c) = b / c
                let
                    lhs =
                        Natural.mul a b |> Natural.divBy (Natural.mul a c)

                    rhs =
                        b |> Natural.divBy c
                in
                lhs |> Expect.equal rhs
        --
        -- , fuzz3 natural natural natural "distributes over addition" <|
        --     \a b c ->
        --         -- (a + b) / c = a / c + b / c
        --         let
        --             lhs =
        --                 Natural.add a b |> Natural.divBy c
        --
        --             x =
        --                 a |> Natural.divBy c
        --
        --             y =
        --                 b |> Natural.divBy c
        --
        --             rhs =
        --                 Maybe.map2 Natural.add x y
        --         in
        --         lhs |> Expect.equal rhs
        --
        -- This is not true in general for division over the natural numbers.
        --
        -- For e.g. try a = 1, b = 1, and c = 2.
        --
        ]


exponentiationSuite : Test
exponentiationSuite =
    describe "exponentiation"
        [ test "0 ^ 0 = 1" <|
            \_ ->
                Natural.exp Natural.zero Natural.zero
                    |> Expect.equal Natural.one
        , fuzz positiveNatural "∀ a ∊ ℕ+, a ^ 0 = 1" <|
            \a ->
                Natural.exp a Natural.zero
                    |> Expect.equal Natural.one
        , fuzz natural "∀ a ∊ ℕ, a ^ 1 = a" <|
            \a ->
                Natural.exp a Natural.one
                    |> Expect.equal a
        , fuzz positiveNatural "∀ n ∊ ℕ+, 0 ^ n = 0" <|
            \n ->
                Natural.exp Natural.zero n
                    |> Expect.equal Natural.zero
        , fuzz3
            baseNatural
            exponentNatural
            exponentNatural
            "product of powers (same base)" <|
            \a m n ->
                -- a^m * a^n = a^{m+n}
                let
                    lhs =
                        Natural.mul (Natural.exp a m) (Natural.exp a n)

                    rhs =
                        Natural.exp a (Natural.add m n)
                in
                lhs |> Expect.equal rhs
        , fuzz3
            baseNatural
            baseNatural
            exponentNatural
            "product of powers (same exponent)" <|
            \a b n ->
                -- a^n * b^n = (a * b)^n
                let
                    lhs =
                        Natural.mul (Natural.exp a n) (Natural.exp b n)

                    rhs =
                        Natural.exp (Natural.mul a b) n
                in
                lhs |> Expect.equal rhs
        , fuzz3
            basePositiveNatural
            exponentNatural
            exponentNatural
            "quotient of powers (same base)" <|
            \a m n ->
                -- a^m / a^n = a^{m-n}
                let
                    lhs =
                        Natural.exp a m |> Natural.divBy (Natural.exp a n)
                in
                if m |> Natural.isGreaterThanOrEqual n then
                    let
                        rhs =
                            Just <| Natural.exp a (Natural.sub m n)
                    in
                    lhs |> Expect.equal rhs

                else if a == Natural.one then
                    lhs |> Expect.equal (Just Natural.one)

                else
                    lhs |> Expect.equal (Just Natural.zero)
        , fuzz3
            baseNatural
            exponentNatural
            exponentNatural
            "power of powers" <|
            \a m n ->
                -- (a^m)^n = a^{m*n}
                let
                    lhs =
                        Natural.exp (Natural.exp a m) n

                    rhs =
                        Natural.exp a (Natural.mul m n)
                in
                lhs |> Expect.equal rhs
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


positiveNatural : Fuzzer Natural
positiveNatural =
    Fuzz.map (Natural.add Natural.one) natural


-- Returns a reasonably sized natural number for the base of an exponentiation.
baseNatural : Fuzzer Natural
baseNatural =
    Fuzz.uniformInt 100
        |> Fuzz.andThen
            (\i ->
                case Natural.fromInt i of
                    Just n ->
                        Fuzz.constant n

                    Nothing ->
                        -- This should NEVER happen.
                        Fuzz.invalid <| "baseNatural: an unexpected error"
            )


basePositiveNatural : Fuzzer Natural
basePositiveNatural =
    baseNatural
        |> Fuzz.andThen
            (\n ->
                Fuzz.constant <|
                    if Natural.isZero n then
                        Natural.one

                    else
                        n
            )


-- Returns a reasonably sized natural number for the exponent (power) of an
-- exponentiation.
exponentNatural : Fuzzer Natural
exponentNatural =
    Fuzz.uniformInt 50
        |> Fuzz.andThen
            (\i ->
                case Natural.fromInt i of
                    Just n ->
                        Fuzz.constant n

                    Nothing ->
                        -- This should NEVER happen.
                        Fuzz.invalid <| "exponentNatural: an unexpected error"
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
