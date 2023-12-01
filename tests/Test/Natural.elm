module Test.Natural exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Natural as N exposing (Natural)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "Natural"
        [ constantsSuite
        , fromIntSuite
        , fromSafeIntSuite
        , fromBaseBStringSuite
        , fromStringSuite
        , toIntSuite
        , stringConversionSuite
        , comparisonSuite
        , maxMinSuite
        , predicatesSuite
        , additionSuite
        , subtractionSuite
        , multiplicationSuite
        , divisionWithRemainderSuite
        , divisionSuite
        , exponentiationSuite
        ]


constantsSuite : Test
constantsSuite =
    let
        expectEqual i n =
            test (String.fromInt i) <|
                \_ ->
                    n |> Expect.equal (N.fromSafeInt i)
    in
    describe "constants"
        [ N.zero |> expectEqual 0
        , N.one |> expectEqual 1
        , N.two |> expectEqual 2
        , N.three |> expectEqual 3
        , N.four |> expectEqual 4
        , N.five |> expectEqual 5
        , N.six |> expectEqual 6
        , N.seven |> expectEqual 7
        , N.eight |> expectEqual 8
        , N.nine |> expectEqual 9
        , N.ten |> expectEqual 10
        ]


fromIntSuite : Test
fromIntSuite =
    describe "fromInt"
        [ test "0" <|
            \_ ->
                N.fromInt 0
                    |> Expect.equal (Just N.zero)
        , test "1" <|
            \_ ->
                N.fromInt 1
                    |> Expect.equal (Just N.one)
        , test "maxSafeInt" <|
            \_ ->
                N.fromInt N.maxSafeInt
                    |> Expect.equal (N.fromString "9007199254740991")
        , test "-1" <|
            \_ ->
                N.fromInt -1
                    |> Expect.equal Nothing
        , fuzz negativeInt "for all negative Ints" <|
            \n ->
                N.fromInt n
                    |> Expect.equal Nothing
        , test "maxSafeInt + 1" <|
            \_ ->
                N.fromInt (N.maxSafeInt + 1)
                    |> Expect.equal Nothing
        , describe "for all safe Ints, s, toInt (fromInt s) == s"
            [ test "maxSafeInt" <|
                \_ ->
                    N.fromInt N.maxSafeInt
                        |> Maybe.map N.toInt
                        |> Expect.equal (Just N.maxSafeInt)
            , fuzz safeInt "safe Ints" <|
                \s ->
                    N.fromInt s
                        |> Maybe.map N.toInt
                        |> Expect.equal (Just s)
            ]
        ]


fromSafeIntSuite : Test
fromSafeIntSuite =
    describe "fromSafeInt"
        [ test "0" <|
            \_ ->
                N.fromSafeInt 0
                    |> Expect.equal N.zero
        , test "1" <|
            \_ ->
                N.fromSafeInt 1
                    |> Expect.equal N.one
        , test "maxSafeInt" <|
            \_ ->
                N.fromSafeInt N.maxSafeInt
                    |> Expect.equal (N.fromSafeString "9007199254740991")
        , test "-1" <|
            \_ ->
                N.fromSafeInt -1
                    |> Expect.equal N.zero
        , test "maxSafeInt + 1" <|
            \_ ->
                N.fromSafeInt (N.maxSafeInt + 1)
                    |> Expect.equal N.zero
        ]


fromBaseBStringSuite : Test
fromBaseBStringSuite =
    describe "fromBaseBString"
        [ test "b=2 1010" <|
            \_ ->
                N.fromBaseBString 2 "1010"
                    |> Expect.equal (Just N.ten)
        , test "b=16 aD" <|
            \_ ->
                N.fromBaseBString 16 "aD"
                    |> Expect.equal (N.fromInt 173)
        , test "b=36 z" <|
            \_ ->
                N.fromBaseBString 36 "z"
                    |> Expect.equal (N.fromInt 35)
        , test "b=2 empty" <|
            \_ ->
                N.fromBaseBString 2 ""
                    |> Expect.equal Nothing
        , test "b=10 A" <|
            \_ ->
                N.fromBaseBString 10 "A"
                    |> Expect.equal Nothing
        , fuzz baseBString "base-b string" <|
            \( b, s ) ->
                N.fromBaseBString b s
                    |> Maybe.andThen (N.toBaseBString b)
                    |> Expect.equal (Just <| toCanonicalBaseBString s)
        ]


fromStringSuite : Test
fromStringSuite =
    describe "fromString"
        [ test "0b10101101" <|
            \_ ->
                N.fromString "0b10101101"
                    |> Expect.equal (N.fromInt 173)
        , test "0o255" <|
            \_ ->
                N.fromString "0o255"
                    |> Expect.equal (N.fromInt 173)
        , test "0XaD" <|
            \_ ->
                N.fromString "0XaD"
                    |> Expect.equal (N.fromInt 173)
        , test "173" <|
            \_ ->
                N.fromString "173"
                    |> Expect.equal (N.fromInt 173)
        , test "b10101101" <|
            \_ ->
                N.fromString "b10101101"
                    |> Expect.equal Nothing
        , test "aD" <|
            \_ ->
                N.fromString "aD"
                    |> Expect.equal Nothing
        , test "0x" <|
            \_ ->
                N.fromString "0x"
                    |> Expect.equal Nothing
        ]


toIntSuite : Test
toIntSuite =
    describe "toInt"
        [ test "0" <|
            \_ ->
                N.toInt N.zero
                    |> Expect.equal 0
        , test "10" <|
            \_ ->
                N.toInt N.ten
                    |> Expect.equal 10
        , test "maxSafeInt" <|
            \_ ->
                N.toInt (N.fromSafeInt N.maxSafeInt)
                    |> Expect.equal N.maxSafeInt
        , test "maxSafeInt + 1" <|
            \_ ->
                N.toInt (N.add (N.fromSafeInt N.maxSafeInt) N.one)
                    |> Expect.equal 0
        , test "maxSafeInt + 10" <|
            \_ ->
                N.toInt (N.add (N.fromSafeInt N.maxSafeInt) N.ten)
                    |> Expect.equal 9
        , fuzz positiveSafeInt "maxSafeInt + n" <|
            \n ->
                N.toInt (N.add (N.fromSafeInt N.maxSafeInt) (N.fromSafeInt n))
                    |> Expect.equal (n - 1)
        ]


stringConversionSuite : Test
stringConversionSuite =
    describe "binary, octal, decimal, and hexadecimal string conversions"
        [ fuzz natural "∀ n ∊ ℕ, fromBinaryString (toBinaryString n) == Just n" <|
            \n ->
                N.fromBinaryString (N.toBinaryString n)
                    |> Expect.equal (Just n)
        , fuzz natural "∀ n ∊ ℕ, fromOctalString (toOctalString n) == Just n" <|
            \n ->
                N.fromOctalString (N.toOctalString n)
                    |> Expect.equal (Just n)
        , fuzz natural "∀ n ∊ ℕ, fromDecimalString (toDecimalString n) == Just n" <|
            \n ->
                N.fromDecimalString (N.toDecimalString n)
                    |> Expect.equal (Just n)
        , fuzz natural "∀ n ∊ ℕ, fromHexString (toHexString n) == Just n" <|
            \n ->
                N.fromHexString (N.toHexString n)
                    |> Expect.equal (Just n)
        ]


comparisonSuite : Test
comparisonSuite =
    describe "comparison"
        [ fuzz2 safeInt safeInt "for all safe Ints, a and b, compare a b = compare (N.fromSafeInt a) (N.fromSafeInt b)" <|
            \a b ->
                let
                    x =
                        N.fromSafeInt a

                    y =
                        N.fromSafeInt b
                in
                compare a b
                    |> Expect.equal (N.compare x y)
        , fuzz natural "∀ n ∊ ℕ, n == n" <|
            \n ->
                N.compare n n
                    |> Expect.equal EQ
        , fuzz natural "∀ n ∊ ℕ, n + 1 > n" <|
            \n ->
                N.compare (N.add n N.one) n
                    |> Expect.equal GT
        , fuzz positiveNatural "∀ n ∊ ℕ+, n - 1 < n" <|
            \n ->
                N.compare (N.sub n N.one) n
                    |> Expect.equal LT
        , test "2 < 5" <|
            \_ ->
                (N.two |> N.isLessThan N.five)
                    |> Expect.equal True
        , test "2 <= 2" <|
            \_ ->
                (N.two |> N.isLessThanOrEqual N.two)
                    |> Expect.equal True
        , test "10 > 5" <|
            \_ ->
                (N.ten |> N.isGreaterThan N.five)
                    |> Expect.equal True
        , test "10 >= 10" <|
            \_ ->
                (N.ten |> N.isGreaterThanOrEqual N.ten)
                    |> Expect.equal True
        ]


maxMinSuite : Test
maxMinSuite =
    describe "max / min"
        [ fuzz2 natural natural "∀ a, b ∊ ℕ, if a < b then max a b == b else max a b == a" <|
            \a b ->
                if a |> N.isLessThan b then
                    N.max a b
                        |> Expect.equal b

                else
                    N.max a b
                        |> Expect.equal a
        , fuzz2 natural natural "∀ a, b ∊ ℕ, if a > b then min a b == b else min a b == a" <|
            \a b ->
                if a |> N.isGreaterThan b then
                    N.min a b
                        |> Expect.equal b

                else
                    N.min a b
                        |> Expect.equal a
        , test "max 5 10 == 10" <|
            \_ ->
                N.max N.five N.ten
                    |> Expect.equal N.ten
        , test "max 10 5 == 10" <|
            \_ ->
                N.max N.ten N.five
                    |> Expect.equal N.ten
        , test "min 5 10 == 5" <|
            \_ ->
                N.min N.five N.ten
                    |> Expect.equal N.five
        , test "min 10 5 == 5" <|
            \_ ->
                N.min N.ten N.five
                    |> Expect.equal N.five
        ]


predicatesSuite : Test
predicatesSuite =
    describe "predicates"
        [ describe "isZero"
            [ fuzz safeInt "if the Int is 0 then true else false" <|
                \s ->
                    let
                        n =
                            N.fromSafeInt s
                    in
                    if s == 0 then
                        N.isZero n
                            |> Expect.equal True

                    else
                        N.isZero n
                            |> Expect.equal False
            ]
        , describe "isOne"
            [ fuzz safeInt "if the Int is 1 then true else false" <|
                \s ->
                    let
                        n =
                            N.fromSafeInt s
                    in
                    if s == 1 then
                        N.isOne n
                            |> Expect.equal True

                    else
                        N.isOne n
                            |> Expect.equal False
            ]
        , describe "isPositive"
            [ fuzz safeInt "if the Int is 0 then false else true" <|
                \s ->
                    let
                        n =
                            N.fromSafeInt s
                    in
                    if s == 0 then
                        N.isPositive n
                            |> Expect.equal False

                    else
                        N.isPositive n
                            |> Expect.equal True
            ]
        , describe "isEven / isOdd"
            [ fuzz safeInt "if the Int is even/odd then the Natural is even/odd" <|
                \s ->
                    let
                        n =
                            N.fromSafeInt s
                    in
                    if isEven s then
                        N.isEven n
                            |> Expect.equal True

                    else
                        N.isOdd n
                            |> Expect.equal True
            , fuzz natural "∀ n ∊ ℕ, n is even iff n + 1 is odd" <|
                \n ->
                    if N.isEven n then
                        N.isOdd (N.add n N.one)
                            |> Expect.equal True

                    else
                        ( N.isOdd n
                        , N.isEven (N.add n N.one)
                        )
                            |> Expect.equal ( True, True )
            ]
        ]


additionSuite : Test
additionSuite =
    describe "addition"
        [ fuzz natural "0 is the identity" <|
            \n ->
                --
                -- n + 0 = n = 0 + n
                --
                ( N.add n N.zero
                , N.add N.zero n
                )
                    |> Expect.equal ( n, n )
        , fuzz2 natural natural "is commutative" <|
            \a b ->
                --
                -- a + b = b + a
                --
                N.add a b
                    |> Expect.equal (N.add b a)
        , fuzz3 natural natural natural "is associative" <|
            \a b c ->
                --
                -- (a + b) + c = a + (b + c)
                --
                N.add (N.add a b) c
                    |> Expect.equal (N.add a (N.add b c))
        ]


subtractionSuite : Test
subtractionSuite =
    describe "(saturating) subtraction"
        [ fuzz natural "∀ n ∊ ℕ, n - 0 = n" <|
            \n ->
                N.sub n N.zero
                    |> Expect.equal n
        , fuzz natural "∀ n ∊ ℕ, 0 - n = 0" <|
            \n ->
                N.sub N.zero n
                    |> Expect.equal N.zero
        , fuzz natural "∀ n ∊ ℕ, n - n = 0" <|
            \n ->
                N.sub n n
                    |> Expect.equal N.zero
        , fuzz2 natural natural "the definition" <|
            \a b ->
                let
                    c =
                        N.sub a b
                in
                if a |> N.isGreaterThanOrEqual b then
                    --
                    -- c + b = a
                    --
                    N.add c b
                        |> Expect.equal a

                else
                    --
                    -- c = 0
                    --
                    c |> Expect.equal N.zero
        ]


multiplicationSuite : Test
multiplicationSuite =
    describe "multiplication"
        [ fuzz natural "1 is the identity" <|
            \n ->
                --
                -- n * 1 = n = 1 * n
                --
                ( N.mul n N.one
                , N.mul N.one n
                )
                    |> Expect.equal ( n, n )
        , fuzz natural "∀ n ∊ ℕ, n * 0 = 0 = 0 * n" <|
            \n ->
                ( N.mul n N.zero
                , N.mul N.zero n
                )
                    |> Expect.equal ( N.zero, N.zero )
        , fuzz2 natural natural "is commutative" <|
            \a b ->
                --
                -- a * b = b * a
                --
                N.mul a b
                    |> Expect.equal (N.mul b a)
        , fuzz3 natural natural natural "is associative" <|
            \a b c ->
                --
                -- (a * b) * c = a * (b * c)
                --
                N.mul (N.mul a b) c
                    |> Expect.equal (N.mul a (N.mul b c))
        , fuzz3 natural natural natural "left-distributive over addition" <|
            \a b c ->
                --
                -- a * (b + c) = a * b + a * c
                --
                N.mul a (N.add b c)
                    |> Expect.equal
                        (N.add (N.mul a b) (N.mul a c))
        , fuzz3 natural natural natural "right-distributive over addition" <|
            \a b c ->
                --
                -- (b + c) * a = b * a + c * a
                --
                N.mul (N.add b c) a
                    |> Expect.equal
                        (N.add (N.mul b a) (N.mul c a))
        ]


divisionWithRemainderSuite : Test
divisionWithRemainderSuite =
    describe "division with remainder"
        [ fuzz2 natural natural "the definition" <|
            \a b ->
                case a |> N.divModBy b of
                    Just ( q, r ) ->
                        if a |> N.isLessThan b then
                            ( q, r )
                                |> Expect.equal ( N.zero, a )

                        else if a == b then
                            ( q, r )
                                |> Expect.equal ( N.one, N.zero )

                        else
                            --
                            -- q * b + r = a
                            --
                            N.add (N.mul q b) r
                                |> Expect.equal a

                    Nothing ->
                        b |> Expect.equal N.zero
        , fuzz natural "by 0 is undefined" <|
            \n ->
                n
                    |> N.divModBy N.zero
                    |> Expect.equal Nothing
        , fuzz natural "by 1" <|
            \n ->
                n
                    |> N.divModBy N.one
                    |> Expect.equal (Just ( n, N.zero ))
        , fuzz natural "by 2 and isEven / isOdd relation" <|
            \n ->
                case n |> N.divModBy N.two of
                    Just ( _, r ) ->
                        if r == N.zero then
                            N.isEven n
                                |> Expect.equal True

                        else
                            ( r, N.isOdd n )
                                |> Expect.equal ( N.one, True )

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
            "cancel common factor"
          <|
            \a b c ->
                --
                -- (a * b) / (a * c) = b / c
                --
                let
                    lhs =
                        N.mul a b |> N.divBy (N.mul a c)

                    rhs =
                        b |> N.divBy c
                in
                lhs |> Expect.equal rhs
        ]


exponentiationSuite : Test
exponentiationSuite =
    describe "exponentiation"
        [ test "0 ^ 0 = 1" <|
            \_ ->
                N.exp N.zero N.zero
                    |> Expect.equal N.one
        , fuzz natural "∀ a ∊ ℕ, a ^ 0 = 1" <|
            \a ->
                N.exp a N.zero
                    |> Expect.equal N.one
        , fuzz natural "∀ a ∊ ℕ, a ^ 1 = a" <|
            \a ->
                N.exp a N.one
                    |> Expect.equal a
        , fuzz positiveNatural "∀ n ∊ ℕ+, 0 ^ n = 0" <|
            \n ->
                N.exp N.zero n
                    |> Expect.equal N.zero
        , fuzz3
            baseNatural
            exponentNatural
            exponentNatural
            "product of powers (same base)"
          <|
            \a m n ->
                --
                -- a^m * a^n = a^{m+n}
                --
                let
                    lhs =
                        N.mul (N.exp a m) (N.exp a n)

                    rhs =
                        N.exp a (N.add m n)
                in
                lhs |> Expect.equal rhs
        , fuzz3
            baseNatural
            baseNatural
            exponentNatural
            "product of powers (same exponent)"
          <|
            \a b n ->
                --
                -- a^n * b^n = (a * b)^n
                --
                let
                    lhs =
                        N.mul (N.exp a n) (N.exp b n)

                    rhs =
                        N.exp (N.mul a b) n
                in
                lhs |> Expect.equal rhs
        , fuzz3
            basePositiveNatural
            exponentNatural
            exponentNatural
            "quotient of powers (same base)"
          <|
            \a m n ->
                --
                -- a^m / a^n = a^{m-n}
                --
                let
                    lhs =
                        N.exp a m |> N.divBy (N.exp a n)
                in
                if m |> N.isGreaterThanOrEqual n then
                    let
                        rhs =
                            Just <| N.exp a (N.sub m n)
                    in
                    lhs |> Expect.equal rhs

                else if a == N.one then
                    lhs |> Expect.equal (Just N.one)

                else
                    lhs |> Expect.equal (Just N.zero)
        , fuzz3
            baseNatural
            exponentNatural
            exponentNatural
            "power of powers"
          <|
            \a m n ->
                --
                -- (a^m)^n = a^{m*n}
                --
                let
                    lhs =
                        N.exp (N.exp a m) n

                    rhs =
                        N.exp a (N.mul m n)
                in
                lhs |> Expect.equal rhs
        ]



-- CUSTOM FUZZERS


safeInt : Fuzzer Int
safeInt =
    Fuzz.intRange 0 N.maxSafeInt


positiveSafeInt : Fuzzer Int
positiveSafeInt =
    Fuzz.intRange 1 N.maxSafeInt


negativeInt : Fuzzer Int
negativeInt =
    Fuzz.intAtMost -1


natural : Fuzzer Natural
natural =
    baseBString
        |> Fuzz.andThen
            (\( b, s ) ->
                case N.fromBaseBString b s of
                    Just n ->
                        Fuzz.constant n

                    Nothing ->
                        --
                        -- This should NEVER happen if both baseBString and
                        -- N.fromBaseBString are written correctly.
                        --
                        Fuzz.invalid <| "natural: an unexpected error"
            )


positiveNatural : Fuzzer Natural
positiveNatural =
    Fuzz.map (N.add N.one) natural


baseNatural : Fuzzer Natural
baseNatural =
    --
    -- Return a reasonably sized natural number
    -- for the base of an exponentiation.
    --
    Fuzz.uniformInt 100
        |> Fuzz.andThen (Fuzz.constant << N.fromSafeInt)


basePositiveNatural : Fuzzer Natural
basePositiveNatural =
    baseNatural
        |> Fuzz.andThen
            (\n ->
                Fuzz.constant <|
                    if N.isZero n then
                        N.one

                    else
                        n
            )


exponentNatural : Fuzzer Natural
exponentNatural =
    --
    -- Return a reasonably sized natural number
    -- for the exponent (power) of an exponentiation.
    --
    Fuzz.uniformInt 50
        |> Fuzz.andThen (Fuzz.constant << N.fromSafeInt)


baseBString : Fuzzer ( Int, String )
baseBString =
    --
    -- Generate random base b (2 <= b <= 36) strings
    -- of at least 1 character and at most 100 characters.
    --
    Fuzz.intRange 2 36
        |> Fuzz.andThen
            (\b ->
                Fuzz.listOfLengthBetween 1 100 (baseBChar b)
                    |> Fuzz.map
                        (\l ->
                            ( b, String.fromList l )
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
                            (if modBy 2 offset == 0 then
                                0x61

                             else
                                0x41
                            )
                                + offset
                                - 10
                )

    else
        Fuzz.invalid "baseBChar: the base must be between 2 and 36 inclusive"



-- HELPERS


toCanonicalBaseBString : String -> String
toCanonicalBaseBString =
    removeLeadingZeros >> String.toUpper


removeLeadingZeros : String -> String
removeLeadingZeros s =
    case String.uncons s of
        Just ( _, "" ) ->
            s

        Just ( '0', t ) ->
            removeLeadingZeros t

        _ ->
            s


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0
