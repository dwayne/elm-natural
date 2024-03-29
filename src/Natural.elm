module Natural exposing
    ( Natural
    , zero, one, two, three, four, five, six, seven, eight, nine, ten
    , maxSafeInt
    , fromSafeInt, fromInt, fromSafeString, fromString, fromBinaryString, fromOctalString, fromDecimalString, fromHexString, fromBaseBString
    , compare, isLessThan, isLessThanOrEqual, isGreaterThan, isGreaterThanOrEqual, max, min
    , isZero, isOne, isPositive, isEven, isOdd
    , add, sub, mul, divModBy, divBy, modBy, exp
    , toInt, toString, toBinaryString, toOctalString, toDecimalString, toHexString, toBaseBString
    )

{-| Compute with the [natural numbers](https://en.wikipedia.org/wiki/Natural_number), ℕ = { 0, 1, 2, ... }.


# Representation

@docs Natural


# Constants

The natural numbers from [`0`](#zero) to [`10`](#ten) inclusive are named.

@docs zero, one, two, three, four, five, six, seven, eight, nine, ten


# Limits

Let `n : Int`. If `0 <= n <= maxSafeInt` then `n` is called a **safe `Int`**.

@docs maxSafeInt


# Constructors

@docs fromSafeInt, fromInt, fromSafeString, fromString, fromBinaryString, fromOctalString, fromDecimalString, fromHexString, fromBaseBString


# Comparison

To test for equality between two natural numbers you can use `==` and `/=`.

    add two two == four

    mul three three /= six

For all other comparisons you will have to use the functions below.

@docs compare, isLessThan, isLessThanOrEqual, isGreaterThan, isGreaterThanOrEqual, max, min


# Predicates

@docs isZero, isOne, isPositive, isEven, isOdd


# Arithmetic

@docs add, sub, mul, divModBy, divBy, modBy, exp


# Conversion

@docs toInt, toString, toBinaryString, toOctalString, toDecimalString, toHexString, toBaseBString

-}

import Bitwise



-- REPRESENTATION


base : Int
base =
    --
    -- Constraints:
    --
    -- base-1 >= 36 (fromBaseBString)
    -- base <= 2^26 (sdMul, sdDivMod)
    --
    2 ^ numBits


numBits : Int
numBits =
    26


baseMask : Int
baseMask =
    base - 1


{-| The largest `Int`, currently `2^53 - 1 = 9007199254740991`, which can be given as input to
[`fromSafeInt`](#fromSafeInt) and [`fromInt`](#fromInt) without causing problems.
-}
maxSafeInt : Int
maxSafeInt =
    2 ^ maxBits - 1


maxBits : Int
maxBits =
    53


{-| A representation of the natural numbers.

**N.B.** _The size of the numbers you can compute with is only limited by the
available memory._

-}
type
    Natural
    --
    -- x = x_{n-1} * base^{n-1} + ... + x_2 * base^2 + x_1 * base + x_0
    --
    -- is represented as
    --
    --   Natural [x_0, x_1, x_2, ..., x_{n-1}]
    --
    -- such that 0 <= x_i < base and 0 < x_{n-1} < base (i.e. no leading zeros).
    --
    -- If x == 0 then it is represented as
    --
    --   Natural []
    --
    = Natural (List Int)



-- CONSTANTS


{-| The natural number [`0`](https://en.wikipedia.org/wiki/0).

To be more precise, it is a representation of the natural number `0`. However, I
will not have any cause to make that distinction. A similar remark can be made
about the other constants.

-}
zero : Natural
zero =
    Natural []


{-| The natural number [`1`](https://en.wikipedia.org/wiki/1).
-}
one : Natural
one =
    Natural [ 1 ]


{-| The natural number [`2`](https://en.wikipedia.org/wiki/2).
-}
two : Natural
two =
    Natural [ 2 ]


{-| The natural number [`3`](https://en.wikipedia.org/wiki/3).
-}
three : Natural
three =
    Natural [ 3 ]


{-| The natural number [`4`](https://en.wikipedia.org/wiki/4).
-}
four : Natural
four =
    Natural [ 4 ]


{-| The natural number [`5`](https://en.wikipedia.org/wiki/5).
-}
five : Natural
five =
    Natural [ 5 ]


{-| The natural number [`6`](https://en.wikipedia.org/wiki/6).
-}
six : Natural
six =
    Natural [ 6 ]


{-| The natural number [`7`](https://en.wikipedia.org/wiki/7).
-}
seven : Natural
seven =
    Natural [ 7 ]


{-| The natural number [`8`](https://en.wikipedia.org/wiki/8).
-}
eight : Natural
eight =
    Natural [ 8 ]


{-| The natural number [`9`](https://en.wikipedia.org/wiki/9).
-}
nine : Natural
nine =
    Natural [ 9 ]


{-| The natural number [`10`](https://en.wikipedia.org/wiki/10).
-}
ten : Natural
ten =
    Natural [ 10 ]



-- CONSTRUCTORS


{-| Create the natural number that represents the given `Int`.

    fromInt 0 == Just zero

    fromInt 1 == Just one

    fromInt maxSafeInt == fromString "9007199254740991"

Unless the given `Int` is negative or greater than [`maxSafeInt`](#maxSafeInt).

    fromInt -1 == Nothing

    fromInt (maxSafeInt + 1) == Nothing

-}
fromInt : Int -> Maybe Natural
fromInt x =
    if x >= 0 && x <= maxSafeInt then
        Just <| Natural <| fromIntHelper [] x

    else
        Nothing


{-| Use this function when you know the given `Int` is a [safe `Int`](#limits).

    fromSafeInt 0 == zero

    fromSafeInt 1 == one

    fromSafeInt maxSafeInt == fromSafeString "9007199254740991"

If the given `Int` isn't safe then [zero](#zero) is returned.

    fromSafeInt -1 == zero

    fromSafeInt (maxSafeInt + 1) == zero

This function is useful for establising **small constants** in a calculation.
For e.g. to [compute the first 100 digits of π using John Machin's formula](https://en.wikipedia.org/wiki/Machin-like_formula)
the natural number 239 is needed.

    twoThirtyNine : Natural
    twoThirtyNine =
        fromSafeInt 239

-}
fromSafeInt : Int -> Natural
fromSafeInt =
    --
    -- x is a safe Int iff
    -- there is an n ∊ ℕ such that fromInt x == Just n.
    --
    fromInt >> Maybe.withDefault zero


fromIntHelper : List Int -> Int -> List Int
fromIntHelper digitsBE n =
    if n == 0 then
        List.reverse digitsBE

    else
        let
            ( q, r ) =
                n |> quotientModBy base
        in
        fromIntHelper (r :: digitsBE) q


{-| Create the natural number represented by the given binary string.

```txt
binary ::= [0-1]+
```

For e.g.

    fromBinaryString "0" == Just zero

    fromBinaryString "1010" == Just ten

    fromBinaryString "" == Nothing
    -- Because the string is empty.

    fromBinaryString "2" == Nothing
    -- Because '2' is not a binary digit.

-}
fromBinaryString : String -> Maybe Natural
fromBinaryString =
    fromBaseBString 2


{-| Create the natural number represented by the given octal string.

```txt
octal ::= [0-7]+
```

For e.g.

    fromOctalString "0" == Just zero

    fromOctalString "12" == Just ten

    fromOctalString "" == Nothing
    -- Because the string is empty.

    fromOctalString "8" == Nothing
    -- Because '8' is not an octal digit.

-}
fromOctalString : String -> Maybe Natural
fromOctalString =
    fromBaseBString 8


{-| Create the natural number represented by the given decimal string.

```txt
decimal ::= [0-9]+
```

For e.g.

    fromDecimalString "0" == Just zero

    fromDecimalString "10" == Just ten

    fromDecimalString "" == Nothing
    -- Because the string is empty.

    fromDecimalString "A" == Nothing
    -- Because 'A' is not a decimal digit.

-}
fromDecimalString : String -> Maybe Natural
fromDecimalString =
    fromBaseBString 10


{-| Create the natural number represented by the given hexadecimal string.

```txt
hex ::= [0-9a-fA-F]+
```

For e.g.

    fromHexString "0" == Just zero

    fromHexString "a" == Just ten

    fromHexString "A" == Just ten

    fromHexString "FE" == fromInt 254

    fromHexString "" == Nothing
    -- Because the string is empty.

    fromHexString "5g" == Nothing
    -- Because 'g' is not a hexadecimal digit.

-}
fromHexString : String -> Maybe Natural
fromHexString =
    fromBaseBString 16


{-| Create the natural number represented by the given string.


### Syntax for `fromString` input

```txt
input    ::= unsigned
unsigned ::= ('0b' | '0B') binary
           | ('0o' | '0O') octal
           | ('0x' | '0X') hex
           | decimal
binary   ::= [0-1]+
octal    ::= [0-7]+
hex      ::= [0-9a-fA-F]+
decimal  ::= [0-9]+
```

For e.g.

    fromString "0b10101101" == fromInt 173

    fromString "0o255" == fromInt 173

    fromString "0XaD" == fromInt 173

    fromString "173" == fromInt 173

    fromString "b10101101" == Nothing
    -- Because the leading '0' is missing.

    fromString "aD" == Nothing
    -- Because 'a' is not a decimal digit.

    fromString "0x" == Nothing
    -- Because there must be at least one hexadecimal digit.

-}
fromString : String -> Maybe Natural
fromString input =
    if String.startsWith "0b" input || String.startsWith "0B" input then
        input
            |> String.dropLeft 2
            |> fromBinaryString

    else if String.startsWith "0o" input || String.startsWith "0O" input then
        input
            |> String.dropLeft 2
            |> fromOctalString

    else if String.startsWith "0x" input || String.startsWith "0X" input then
        input
            |> String.dropLeft 2
            |> fromHexString

    else
        fromBaseBString 10 input


{-| It's best to use this function when you can guarantee that the string you're
dealing with is a valid input to the [`fromString`](#fromString) function.

If the input is invalid then [`zero`](#zero) is returned.

**N.B.** _Read the documentation of [`fromString`](#fromString) to learn what's considered
to be valid or invalid input to this function._

This function is useful for establishing **large constants** in a calculation.

    oneGoogol : Natural
    oneGoogol =
        -- 10 ^ 100
        fromSafeString "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

Learn more about a [googol](https://en.wikipedia.org/wiki/Googol).


### What's considered a large constant?

Let `n : Int`, since `fromSafeInt n` can be used for `0 <= n <= maxSafeInt` then it makes sense
to consider any natural number larger than [`maxSafeInt`](#maxSafeInt), a large constant.

-}
fromSafeString : String -> Natural
fromSafeString =
    fromString >> Maybe.withDefault zero


{-| Create the natural number represented by the given base-`b` string.

`b` must be between 2 and 36 inclusive and each character in the string must be
a valid base-`b` digit.


### About base-`b` digits

A valid base-`b` digit is any digit `d` such that `0 <= d <= b - 1`.

For bases larger than 10, we use case-insensitive letters from the [Latin alphabet](https://en.wikipedia.org/wiki/Latin_alphabet)
to represent the base-`b` digits that are 10 or larger. So,

```txt
A or a represents 10
B or b represents 11
C or c represents 12
...
Z or z represents 35
```

For e.g.

If `b = 16` then the valid base-16 digits are `[0-9a-fA-F]`.

If `b = 36` then the valid base-36 digits are `[0-9a-zA-Z]`.


### Syntax for `fromBaseBString` input

```txt
input ::= [digit]+
digit ::= [0-9a-zA-Z]
```

Valid strings when `b = 16`:

`"0"`, `"123"`, and `"Ff"`.

Invalid strings when `b = 16`:

  - `"+2"`, because `'+'` is not part of the allowed syntax,
  - `"5g"`, because `'g'` is not a hexdecimal digit.

For e.g.

    fromBaseBString 2 "1010" == Just ten

    fromBaseBString 16 "aD" == fromInt 173

    fromBaseBString 36 "z" == fromInt 35

    fromBaseBString 2 "" == Nothing
    -- Because the string is empty.

    fromBaseBString 10 "A" == Nothing
    -- Because 'A' is not a decimal digit.

-}
fromBaseBString : Int -> String -> Maybe Natural
fromBaseBString b input =
    -- TODO: Explore chunking for efficiency reasons.
    if isBaseB b && isBaseBString b input then
        Just <|
            Natural <|
                String.foldl
                    -- To satisfy the assumptions of sdAdd and sdMul
                    -- we need base-1 >= b.
                    --
                    (\char x -> sdAdd (sdMul x b) (toBaseBDigit b char))
                    []
                    input

    else
        Nothing


isBaseBString : Int -> String -> Bool
isBaseBString b input =
    --
    -- Assumptions
    --
    -- 1. isBaseB b
    --
    input /= "" && String.all (isBaseBChar b) input


isBaseBChar : Int -> Char -> Bool
isBaseBChar b char =
    --
    -- Assumptions
    --
    -- 1. isBaseB b
    --
    let
        code =
            Char.toCode char
    in
    (0x30 <= code && code <= Basics.min (0x30 + b - 1) 0x39)
        || (0x41 <= code && code <= 0x41 + b - 11)
        || (0x61 <= code && code <= 0x61 + b - 11)


toBaseBDigit : Int -> Char -> Int
toBaseBDigit b char =
    --
    -- Assumptions
    --
    -- 1. isBaseB b
    -- 2. isBaseBChar b char
    --
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= Basics.min (0x30 + b - 1) 0x39 then
        code - 0x30

    else if 0x41 <= code && code <= 0x41 + b - 11 then
        code - 0x41 + 10

    else
        code - 0x61 + 10



-- COMPARISON


{-| Compare any two natural numbers.

    compare three four == LT

    compare four four == EQ

    compare five four == GT

-}
compare : Natural -> Natural -> Order
compare (Natural xsLE) (Natural ysLE) =
    compareLE xsLE ysLE


compareLE : List Int -> List Int -> Order
compareLE xsLE ysLE =
    compareLEHelper 0 0 xsLE ysLE


compareLEHelper : Int -> Int -> List Int -> List Int -> Order
compareLEHelper a b xsLE ysLE =
    case ( xsLE, ysLE ) of
        ( [], [] ) ->
            Basics.compare a b

        ( [], _ ) ->
            LT

        ( _, [] ) ->
            GT

        ( x :: xsLERest, y :: ysLERest ) ->
            if x == y then
                compareLEHelper a b xsLERest ysLERest

            else
                compareLEHelper x y xsLERest ysLERest


{-| Determine if the second natural number is less than the first.

    (two |> isLessThan eight) == True

    (two |> isLessThan two) == False

    (eight |> isLessThan two) == False

-}
isLessThan : Natural -> Natural -> Bool
isLessThan b a =
    -- Is a < b?
    compare a b == LT


{-| Determine if the second natural number is less than or equal to the
first.

    (two |> isLessThanOrEqual eight) == True

    (two |> isLessThanOrEqual two) == True

    (eight |> isLessThanOrEqual two) == False

-}
isLessThanOrEqual : Natural -> Natural -> Bool
isLessThanOrEqual b a =
    --
    -- Is a <= b?
    --
    -- a <= b iff not (a > b)
    --
    not (a |> isGreaterThan b)


{-| Determine if the second natural number is greater than the first.

    (two |> isGreaterThan eight) == False

    (two |> isGreaterThan two) == False

    (eight |> isGreaterThan two) == True

-}
isGreaterThan : Natural -> Natural -> Bool
isGreaterThan b a =
    --
    -- Is a > b?
    --
    compare a b == GT


{-| Determine if the second natural number is greater than or equal to
the first.

    (two |> isGreaterThanOrEqual eight) == False

    (two |> isGreaterThanOrEqual two) == True

    (eight |> isGreaterThanOrEqual two) == True

-}
isGreaterThanOrEqual : Natural -> Natural -> Bool
isGreaterThanOrEqual b a =
    --
    -- Is a >= b?
    --
    -- a >= b iff not (a < b)
    --
    not (a |> isLessThan b)


{-| Find the larger of two natural numbers.

    max five ten == ten

    max ten five == ten

-}
max : Natural -> Natural -> Natural
max a b =
    if a |> isGreaterThanOrEqual b then
        a

    else
        b


{-| Find the smaller of two natural numbers.

    min five ten == five

    min ten five == five

-}
min : Natural -> Natural -> Natural
min a b =
    if a |> isLessThanOrEqual b then
        a

    else
        b



-- PREDICATES


{-| Determine if the natural number is [`0`](#zero).

    isZero zero == True

    isZero one == False

-}
isZero : Natural -> Bool
isZero =
    (==) zero


{-| Determine if the natural number is [`1`](#one).

    isOne zero == False

    isOne one == True

-}
isOne : Natural -> Bool
isOne =
    (==) one


{-| Determine if the natural number is positive (i.e. greater than [`0`](#zero)).

    isPositive five == True

    isPositive zero == False

-}
isPositive : Natural -> Bool
isPositive =
    not << isZero


{-| Determine if the natural number is even (i.e. divisible by [`2`](#two)).

    isEven zero == True

    isEven one == False

    isEven two == True

    isEven three == False

-}
isEven : Natural -> Bool
isEven (Natural digitsLE) =
    case digitsLE of
        [] ->
            True

        d :: _ ->
            Basics.modBy 2 d == 0


{-| Determine if the natural number is odd (i.e. not even, so not divisible by [`2`](#two)).

    isOdd zero == False

    isOdd one == True

    isOdd two == False

    isOdd three == True

-}
isOdd : Natural -> Bool
isOdd =
    not << isEven



-- ARITHMETIC


{-| Add two natural numbers.
-}
add : Natural -> Natural -> Natural
add (Natural xsLE) (Natural ysLE) =
    Natural <| addHelper xsLE ysLE 0 []


addHelper : List Int -> List Int -> Int -> List Int -> List Int
addHelper xsLE ysLE carry zsBE =
    --
    -- Assumptions
    --
    -- 1. xsLE = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. ysLE = [ y_0, y_1, ..., y_m ] (LE) and 0 <= yi <= base-1
    -- 3. carry = 0 or 1
    -- 4. zsBE = [ z_k, ..., z_1, z_0 ] (BE) and 0 <= zi <= base-1
    --
    case ( xsLE, ysLE ) of
        ( [], [] ) ->
            let
                finalZsBE =
                    if carry == 0 then
                        zsBE

                    else
                        carry :: zsBE
            in
            List.reverse finalZsBE

        ( x :: restXsLE, [] ) ->
            let
                ( newCarry, z ) =
                    x + carry |> iDivModBy base
            in
            addHelper restXsLE [] newCarry (z :: zsBE)

        ( [], y :: restYsLE ) ->
            let
                ( newCarry, z ) =
                    y + carry |> iDivModBy base
            in
            addHelper [] restYsLE newCarry (z :: zsBE)

        ( x :: restXsLE, y :: restYsLE ) ->
            let
                ( newCarry, z ) =
                    x + y + carry |> iDivModBy base
            in
            addHelper restXsLE restYsLE newCarry (z :: zsBE)


{-| Subtract the second natural number from the first.

    sub ten four == six
    -- 10 - 4 = 6

It uses [saturating subtraction](https://en.wikipedia.org/wiki/Saturation_arithmetic).
Thus, if the second natural number is larger than the first, 0 is returned.

    sub four ten == zero
    -- 4 - 10 = 0

-}
sub : Natural -> Natural -> Natural
sub (Natural xsLE) (Natural ysLE) =
    --
    -- Saturating subtraction
    --
    -- x - y = n, where n + y = x and n is a natural number (if x >= y)
    --       = 0                                            (if x < y)
    --
    Natural <| subHelper xsLE ysLE 0 []


subHelper : List Int -> List Int -> Int -> List Int -> List Int
subHelper xsLE ysLE carry zsBE =
    --
    -- Assumptions
    --
    -- 1. xsLE = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. ysLE = [ y_0, y_1, ..., y_m ] (LE) and 0 <= yi <= base-1
    -- 3. carry = 0 or -1
    -- 4. zsBE = [ z_k, ..., z_1, z_0 ] (BE) and 0 <= zi <= base-1
    --
    case ( xsLE, ysLE ) of
        ( [], [] ) ->
            if carry == 0 then
                zsBE
                    |> removeLeadingZeros
                    |> List.reverse

            else
                -- carry == -1 which means xsLE < ysLE
                []

        ( x :: restXsLE, [] ) ->
            let
                ( newCarry, z ) =
                    x + carry |> quotientModBy base
            in
            subHelper restXsLE [] newCarry (z :: zsBE)

        ( [], y :: restYsLE ) ->
            let
                ( newCarry, z ) =
                    carry - y |> quotientModBy base
            in
            subHelper [] restYsLE newCarry (z :: zsBE)

        ( x :: restXsLE, y :: restYsLE ) ->
            let
                ( newCarry, z ) =
                    x - y + carry |> quotientModBy base
            in
            subHelper restXsLE restYsLE newCarry (z :: zsBE)


{-| Multiply two natural numbers.
-}
mul : Natural -> Natural -> Natural
mul (Natural xsLE) (Natural ysLE) =
    Natural <| karatsuba xsLE ysLE


karatsuba : List Int -> List Int -> List Int
karatsuba xsLE ysLE =
    case ( xsLE, ysLE ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( [ 1 ], _ ) ->
            ysLE

        ( _, [ 1 ] ) ->
            xsLE

        _ ->
            let
                xLen =
                    List.length xsLE

                yLen =
                    List.length ysLE
            in
            if xLen < 1000 || yLen < 1000 then
                mulHelper xsLE (List.reverse ysLE) []

            else
                let
                    n =
                        Basics.max xLen yLen

                    m =
                        n // 2

                    m2 =
                        2 * m

                    ( x0, x1 ) =
                        splitAt m xsLE

                    ( y0, y1 ) =
                        splitAt m ysLE

                    z0 =
                        -- x0y0 = x0 * y0
                        karatsuba x0 y0

                    z2 =
                        -- x1y1 = x1 * y1
                        karatsuba x1 y1

                    t1 =
                        -- (x0 + x1)(y0 + y1)
                        karatsuba
                            (addHelper x0 x1 0 [])
                            (addHelper y0 y1 0 [])

                    t2 =
                        -- (x0 + x1)(y0 + y1) - x0y0
                        subHelper t1 z0 0 []

                    z1 =
                        -- (x0 + x1)(y0 + y1) - x0y0 - x1y1
                        subHelper t2 z2 0 []

                    t3 =
                        -- z1 * base^m + z0
                        addHelper (shiftLeftBy m z1) z0 0 []
                in
                -- xy = z2 * base^2m + z1 * base^m + z0
                addHelper (shiftLeftBy m2 z2) t3 0 []


mulHelper : List Int -> List Int -> List Int -> List Int
mulHelper xsLE ysBE zsLE =
    --
    -- Assumptions
    --
    -- 1. xsLE = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. ysBE = [ y_m, ..., y_1, y_0 ] (BE) and 0 <= yi <= base-1
    -- 3. zsLE = [ z_0, z_1, ..., z_k ] (LE) and 0 <= zi <= base-1
    --
    case ysBE of
        [] ->
            zsLE

        y :: restYsBE ->
            let
                -- base * zsLE
                augend =
                    if zsLE == [] then
                        []

                    else
                        0 :: zsLE

                -- xsLE * y
                addend =
                    sdMul xsLE y

                -- (base * zsLE) + (xsLE * y)
                partialSum =
                    addHelper augend addend 0 []
            in
            mulHelper xsLE restYsBE partialSum


shiftLeftBy : Int -> List Int -> List Int
shiftLeftBy n digitsLE =
    case digitsLE of
        [] ->
            []

        _ ->
            shiftLeftByHelper n digitsLE


shiftLeftByHelper : Int -> List Int -> List Int
shiftLeftByHelper n digitsLE =
    if n == 0 then
        digitsLE

    else
        shiftLeftByHelper (n - 1) (0 :: digitsLE)


{-| Find the quotient and remainder when the second natural number (the **dividend**) is divided by
the first (the **divisor**).

This operation performs [Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division)
or **division with remainder**.

`divModBy d D` of two natural numbers `D` and `d ≠ 0`,
is defined as producing two unique natural numbers `q` (the **quotient**) and `r` (the **remainder**)
such that

  - `d * q` is the greatest multiple of `d` less than or equal to `D`, and
  - `r = D - d * q` such that `0 <= r < d`.

For e.g.

    (ten |> divModBy two) == Just (five, zero)
    -- Because 2 * 5 = 10 is the greatest multiple of 2 less than or equal to 10, and
    -- 0 = 10 - 2 * 5 such that 0 <= 0 < 10.

    (ten |> divModBy three) == Just (three, one)
    -- Because 3 * 3 = 9 is the greatest multiple of 3 less than or equal to 10, and
    -- 1 = 10 - 3 *3 such that 0 <= 1 < 10.

Division by [`0`](#zero) is not allowed. So, for all `n : Natural`,

    (n |> divModBy zero) == Nothing

-}
divModBy : Natural -> Natural -> Maybe ( Natural, Natural )
divModBy (Natural ysLE) ((Natural xsLE) as x) =
    case ysLE of
        [] ->
            Nothing

        [ d ] ->
            let
                ( qs, r ) =
                    sdDivMod xsLE d [] 0
            in
            Just
                ( Natural qs
                , if r == 0 then
                    zero

                  else
                    Natural [ r ]
                )

        _ ->
            case compareLE xsLE ysLE of
                LT ->
                    Just ( zero, x )

                EQ ->
                    Just ( one, zero )

                GT ->
                    let
                        -- 2 <= m <= n
                        -- k >= 0
                        ( n, rsBE ) =
                            lengthAndReverse xsLE

                        ( m, dsBE ) =
                            lengthAndReverse ysLE

                        k =
                            n - m

                        ( d2, d2LE ) =
                            computeD2 dsBE

                        ( qsLE, rsLE ) =
                            longDivision rsBE ysLE d2 d2LE m k []
                    in
                    Just ( Natural qsLE, Natural rsLE )


computeD2 : List Int -> ( Int, List Int )
computeD2 digitsBE =
    --
    -- Assumes |digitsBE| >= 2.
    --
    case digitsBE of
        d1 :: d0 :: _ ->
            ( d1 * base + d0
            , [ d0, d1 ]
            )

        _ ->
            ( 0
            , []
            )


longDivision : List Int -> List Int -> Int -> List Int -> Int -> Int -> List Int -> ( List Int, List Int )
longDivision rsBE dsLE d2 d2LE m k qsLE =
    --
    -- A divide and correct method based on the algorithm described in the paper
    -- "Multiple-Length Division Revisited: A Tour of the Minefield" by Per Brinch Hansen.
    --
    if k < 0 then
        ( qsLE
        , rsBE
            |> removeLeadingZeros
            |> List.reverse
        )

    else
        let
            -- k goes from n-m to 0
            -- j goes from n+1 to m+1
            j =
                k + m + 1

            -- z <= 3
            z =
                -- the amount of zero padding
                j - List.length rsBE

            l =
                -- the number of leading digits to use from rsBE to form r3
                3 - z

            -- So, suppose z = 2. Then, the zero-padded version of rsBE
            -- = [0, 0, d, ...]. Hence, we only need to take l = 1 digit
            -- as the prefix of rsBE.
            --
            -- Hence, r3 = 0 * base^2 + 0 * base + d = d.
            r3 =
                rsBE
                    |> List.take l
                    |> prefix3

            p =
                -- the number of leading digits to use from rsBE to form rpBE
                m + 1 - z

            rpBE =
                List.take p rsBE

            rpLE =
                List.reverse rpBE

            qt0 =
                computeFirstEstimation r3 d2 d2LE

            dq0LE =
                sdMul dsLE qt0

            ( qk, dqLE ) =
                if compareLE rpLE dq0LE == LT then
                    -- We need to adjust the estimate.
                    --
                    -- The cool thing about this algorithm is that if our first
                    -- estimate is ever incorrect then the adjusted value is
                    -- correct.
                    --
                    -- Read the paper to see the proof.
                    let
                        qt1 =
                            qt0 - 1
                    in
                    ( qt1
                    , sdMul dsLE qt1
                    )

                else
                    ( qt0
                    , dq0LE
                    )

            rhBE =
                subHelper rpLE dqLE 0 []
                    |> List.reverse

            rtBE =
                List.drop p rsBE

            newRsBE =
                List.append rhBE rtBE
        in
        if qk == 0 && qsLE == [] then
            longDivision newRsBE dsLE d2 d2LE m (k - 1) qsLE

        else
            longDivision newRsBE dsLE d2 d2LE m (k - 1) (qk :: qsLE)


type Prefix3
    = P3Single Int
    | P3Multi (List Int)


prefix3 : List Int -> Prefix3
prefix3 digitsBE =
    case digitsBE of
        [] ->
            P3Single 0

        [ d0 ] ->
            P3Single d0

        [ d1, d0 ] ->
            P3Single (d1 * base + d0)

        0 :: d1 :: d0 :: _ ->
            P3Single (d1 * base + d0)

        d2 :: d1 :: d0 :: _ ->
            P3Multi [ d0, d1, d2 ]


computeFirstEstimation : Prefix3 -> Int -> List Int -> Int
computeFirstEstimation r3 d2 d2LE =
    case r3 of
        P3Single x ->
            Basics.min (x // d2) baseMask

        P3Multi xsLE ->
            case xsLE |> slowDivModBy d2LE of
                ( [], _ ) ->
                    0

                ( [ qk ], _ ) ->
                    qk

                _ ->
                    baseMask


slowDivModBy : List Int -> List Int -> ( List Int, List Int )
slowDivModBy ysLE xsLE =
    case compareLE xsLE ysLE of
        LT ->
            ( [], xsLE )

        EQ ->
            ( [ 1 ], [] )

        GT ->
            let
                twoYLE =
                    sdMul ysLE 2

                ( qsLE, rLE ) =
                    xsLE |> slowDivModBy twoYLE

                twoQsLE =
                    sdMul qsLE 2
            in
            if compareLE rLE ysLE == LT then
                ( twoQsLE
                , rLE
                )

            else
                ( sdAdd twoQsLE 1
                , subHelper rLE ysLE 0 []
                )


{-| Find the quotient when the second natural number is divided by the first.

**N.B.** _Please see [`divModBy`](#divModBy) to understand how the quotient is computed._

-}
divBy : Natural -> Natural -> Maybe Natural
divBy divisor =
    --
    -- Is there a faster algorithm since we don't care about the remainder?
    --
    divModBy divisor >> Maybe.map Tuple.first


{-| Find the remainder when the second natural number is divided by the first.

**N.B.** _Please see [`divModBy`](#divModBy) to understand how the remainder is computed._

-}
modBy : Natural -> Natural -> Maybe Natural
modBy divisor =
    --
    -- Is there a faster algorithm since we don't care about the quotient?
    --
    divModBy divisor >> Maybe.map Tuple.second


{-| Find the power of the first natural number (the **base**) to the second
natural number (the **exponent**).

    exp two three == eight

For all `n : Natural`,

    exp n zero == one

In particular,

    exp zero zero == one

**N.B.** _You can read "[What is `0^0`?](https://maa.org/book/export/html/116806)" to learn more_.

For all `n : Natural`, where `n` is positive,

    exp zero n == zero

-}
exp : Natural -> Natural -> Natural
exp b n =
    if isZero n then
        one

    else if isZero b then
        zero

    else
        expHelper b n one


expHelper : Natural -> Natural -> Natural -> Natural
expHelper b n y =
    if isZero n then
        y

    else
        let
            ( q, r ) =
                n |> divModBy two |> Maybe.withDefault ( one, zero )

            bSquared =
                mul b b
        in
        if r == zero then
            -- n is even
            expHelper bSquared q y

        else
            -- n is odd
            expHelper bSquared q (mul y b)



-- CONVERSION


{-| Convert any natural number, `n`, to `n mod (maxSafeInt + 1)`.

    toInt zero == 0

    toInt ten == 10

    toInt (fromSafeInt maxSafeInt) == maxSafeInt

    toInt (add (fromSafeInt maxSafeInt) one) == 0

    toInt (add (fromSafeInt maxSafeInt) ten) == 9

-}
toInt : Natural -> Int
toInt (Natural digitsLE) =
    case digitsLE of
        [] ->
            0

        _ ->
            let
                ( q, r ) =
                    maxBits |> iDivModBy numBits

                ( len, maskStart ) =
                    if r > 0 then
                        ( q + 1
                        , 2 ^ r - 1
                        )

                    else
                        ( q
                        , baseMask
                        )
            in
            digitsLE
                |> List.take len
                |> List.reverse
                |> padLeft len 0
                |> toIntHelper maskStart 0


toIntHelper : Int -> Int -> List Int -> Int
toIntHelper mask x digitsBE =
    case digitsBE of
        [] ->
            x

        digit :: restDigitsBE ->
            toIntHelper
                baseMask
                (x * base + Bitwise.and digit mask)
                restDigitsBE


{-| Convert any natural number to its binary (base-2) representation.

    toBinaryString zero == "0"

    toBinaryString one == "1"

    toBinaryString ten == "1010"

    toBinaryString (fromSafeInt 1729) == "11011000001"

    toBinaryString (add (fromSafeInt maxSafeInt) one) == "100000000000000000000000000000000000000000000000000000"

-}
toBinaryString : Natural -> String
toBinaryString =
    toBaseBString 2 >> Maybe.withDefault ""


{-| Convert any natural number to its octal (base-8) representation.

    toOctalString zero == "0"

    toOctalString one == "1"

    toOctalString ten == "12"

    toOctalString (fromSafeInt 1729) == "3301"

    toOctalString (add (fromSafeInt maxSafeInt) one) == "400000000000000000"

-}
toOctalString : Natural -> String
toOctalString =
    toBaseBString 8 >> Maybe.withDefault ""


{-| Convert any natural number to its decimal (base-10) representation.

    toDecimalString zero == "0"

    toDecimalString one == "1"

    toDecimalString ten == "10"

    toDecimalString (fromSafeInt 1729) == "1729"

    toDecimalString (add (fromSafeInt maxSafeInt) one) == "9007199254740992"

-}
toDecimalString : Natural -> String
toDecimalString =
    toBaseBString 10 >> Maybe.withDefault ""


{-| Convert any natural number to its hexadecimal (base-16) representation.

    toHexString zero == "0"

    toHexString one == "1"

    toHexString ten == "A"

    toHexString (fromSafeInt 1729) == "6C1"

    toHexString (add (fromSafeInt maxSafeInt) one) == "20000000000000"

-}
toHexString : Natural -> String
toHexString =
    toBaseBString 16 >> Maybe.withDefault ""


{-| An alias for [`toDecimalString`](#toDecimalString).
-}
toString : Natural -> String
toString =
    toDecimalString


{-| Convert any natural number to its base-`b` representation.

`b` must be between 2 and 36 inclusive and each character in the resulting
string will be a valid base-`b` digit.

All [Latin letters](https://en.wikipedia.org/wiki/Latin_alphabet) in the
base-`b` representation will be in uppercase.

For e.g.

    toBaseBString 2 (fromSafeInt 1729) == Just "11011000001"

    toBaseBString 8 (fromSafeInt 1729) == Just "3301"

    toBaseBString 10 (fromSafeInt 1729) == Just "1729"

    toBaseBString 16 (fromSafeInt 1729) == Just "6C1"

    toBaseBString 36 (fromSafeInt 1729) == Just "1C1"

For any `k : Int` where `k < 2` or `k > 36`, and any `n : Natural`,

    toBaseBString k n == Nothing

**N.B.** _Please refer to [`fromBaseBString`](#fromBaseBString) to learn more about
base-`b` digits._

-}
toBaseBString : Int -> Natural -> Maybe String
toBaseBString b (Natural xs) =
    -- TODO: Explore chunking for efficiency reasons.
    if isBaseB b then
        Just <| toBaseBStringHelper b xs ""

    else
        Nothing


toBaseBStringHelper : Int -> List Int -> String -> String
toBaseBStringHelper b xs result =
    case xs of
        [] ->
            if result == "" then
                "0"

            else
                result

        _ ->
            let
                ( q, r ) =
                    sdDivMod xs b [] 0
            in
            toBaseBStringHelper b q (String.cons (toBaseBChar r) result)


toBaseBChar : Int -> Char
toBaseBChar offset =
    --
    -- Assumptions
    --
    -- 1. 0 <= offset <= b-1, where isBaseB b is True for some b
    --
    Char.fromCode <|
        if offset < 10 then
            0x30 + offset

        else
            0x41 + offset - 10


isBaseB : Int -> Bool
isBaseB b =
    2 <= b && b <= 36



-- SINGLE-DIGIT OPERATIONS


sdAdd : List Int -> Int -> List Int
sdAdd xsLE y =
    case ( xsLE, y ) of
        ( _, 0 ) ->
            xsLE

        ( [], _ ) ->
            [ y ]

        _ ->
            sdAddHelper xsLE y []


sdAddHelper : List Int -> Int -> List Int -> List Int
sdAddHelper xsLE y zsBE =
    --
    -- zsBE = xsLE + y
    --
    -- Preconditions:
    --
    -- - xsLE = [ x_0, x_1, ..., x_n ] and 0 <= x_i <= base-1
    -- - 0 <= y <= base-1
    --
    -- Postconditions:
    --
    -- - zsBE = [ z_m, ..., z_1, z_0 ] and 0 <= z_j <= base-1
    --
    if y == 0 then
        sdAddPrepend zsBE xsLE

    else
        case xsLE of
            [] ->
                List.reverse <| y :: zsBE

            x :: restXsLE ->
                let
                    sum =
                        x + y

                    ( carry, z ) =
                        if sum < base then
                            ( 0, sum )

                        else
                            ( 1, sum - base )
                in
                sdAddHelper restXsLE carry (z :: zsBE)


sdAddPrepend : List Int -> List Int -> List Int
sdAddPrepend digitsBE digitsLE =
    case digitsBE of
        [] ->
            digitsLE

        d :: digitsBERest ->
            sdAddPrepend digitsBERest (d :: digitsLE)


sdMul : List Int -> Int -> List Int
sdMul xsLE y =
    case ( xsLE, y ) of
        ( [], _ ) ->
            []

        ( _, 0 ) ->
            []

        ( _, 1 ) ->
            xsLE

        _ ->
            sdMulHelper xsLE y 0 []
                |> List.reverse


sdMulHelper : List Int -> Int -> Int -> List Int -> List Int
sdMulHelper xsLE y carry zsBE =
    --
    -- zsBE = xsLE * y + carry
    --
    -- Preconditions:
    --
    -- - xsLE = [ x_0, x_1, ..., x_n ] and 0 <= x_i <= base-1
    -- - 0 <= y <= base-1
    -- - 0 <= carry <= base-2
    --
    -- Postconditions:
    --
    -- - zsBE = [ z_m, ..., z_1, z_0 ] and 0 <= z_j <= base-1
    --
    case xsLE of
        [] ->
            if carry == 0 then
                zsBE

            else
                carry :: zsBE

        x :: restXsLE ->
            let
                -- product constrains how large the base can be.
                --
                -- 0 <= product <= (base-1)(base-1) + base-2
                --              <= base^2 - base - 1
                --
                -- And, we want
                --
                -- base^2 - base - 1 <= maxSafeInt
                --
                -- Since we want base to be a power of 2, i.e. base = 2^n, then
                --
                -- 2^{2n} - 2^n - 1 <= maxSafeInt
                --                  <= 2^53 - 1
                --     2^{2n} - 2^n <= 2^53
                --
                -- This gives n <= 26, i.e. the maximum base we will be able to
                -- use is 2^26.
                product =
                    x * y + carry

                ( newCarry, z ) =
                    product |> iDivModBy base
            in
            sdMulHelper restXsLE y newCarry (z :: zsBE)


sdDivMod : List Int -> Int -> List Int -> Int -> ( List Int, Int )
sdDivMod xs y qs r =
    --
    -- xs = qs * y + r
    --
    -- Assumptions
    --
    -- 1. xs = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. 0 < y <= base-1
    -- 3. qs = [ q_m, ..., q_1, q_0 ] (BE) and 0 <= qj <= base-1
    -- 4. 0 <= r <= base-1
    --
    sdDivModHelper (List.reverse xs) y True qs r


sdDivModHelper : List Int -> Int -> Bool -> List Int -> Int -> ( List Int, Int )
sdDivModHelper xs y isTrailingZero qs r =
    case xs of
        [] ->
            ( qs, r )

        x :: restXs ->
            let
                -- value constrains how large the base can be.
                --
                -- 0 <= value <= (base-1)*base + base-1
                --            <= base^2 - 1
                --
                -- And, we want
                --
                -- base^2 - 1 <= maxSafeInt
                --
                -- Since we want base to be a power of 2, i.e. base = 2^n, then
                --
                -- 2^{2n} - 1 <= maxSafeInt
                --            <= 2^53 - 1
                --     2^{2n} <= 2^53
                --         2n <= 53
                --
                -- This gives n <= 26, i.e. the maximum base we will be able to
                -- use is 2^26.
                --
                value =
                    r * base + x

                ( q, newR ) =
                    iDivModBy y value
            in
            if isTrailingZero && q == 0 then
                sdDivModHelper restXs y isTrailingZero qs newR

            else
                sdDivModHelper restXs y False (q :: qs) newR



-- HELPERS


iDivModBy : Int -> Int -> ( Int, Int )
iDivModBy divisor dividend =
    ( dividend // divisor
    , Basics.modBy divisor dividend
    )


quotientModBy : Int -> Int -> ( Int, Int )
quotientModBy divisor dividend =
    ( floor (toFloat dividend / toFloat divisor)
    , Basics.modBy divisor dividend
    )


lengthAndReverse : List a -> ( Int, List a )
lengthAndReverse list =
    lengthAndReverseHelper list 0 []


lengthAndReverseHelper : List a -> Int -> List a -> ( Int, List a )
lengthAndReverseHelper list n revList =
    case list of
        [] ->
            ( n, revList )

        x :: restList ->
            lengthAndReverseHelper restList (n + 1) (x :: revList)


padLeft : Int -> a -> List a -> List a
padLeft n x list =
    List.repeat (n - List.length list) x ++ list


removeLeadingZeros : List Int -> List Int
removeLeadingZeros digits =
    case digits of
        [] ->
            []

        d :: restDigits ->
            if d == 0 then
                removeLeadingZeros restDigits

            else
                digits


splitAt : Int -> List Int -> ( List Int, List Int )
splitAt n list =
    ( List.take n list
    , List.drop n list
    )
