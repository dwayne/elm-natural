module Natural exposing
    ( Natural
    , zero, one, two, three, four, five, six, seven, eight, nine, ten
    , fromInt
    , fromBinaryString, fromOctalString, fromHexString, fromString
    , fromBaseBString
    , compare
    , isLessThan, isLessThanOrEqual, isGreaterThan, isGreaterThanOrEqual
    , max, min
    , isZero, isNonZero, isEven, isOdd
    , add, sub, mul, divModBy, exp
    , toInt
    , toBinaryString, toOctalString, toHexString, toString
    , toBaseBString

    -- For testing purposes
    , sdAdd, sdSub, sdMul, sdDivMod
    )


import Bitwise


-- REPRESENTATION


base : Int
base =
    -- Constraints:
    --
    -- base-1 >= 36 (fromBaseBString)
    -- base <= 2^26 (sdMul, sdDivMod)
    2 ^ numBits


numBits : Int
numBits =
    26


baseMask : Int
baseMask =
    base - 1


type Natural
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
    = Natural (List Int)


-- CONSTANTS


zero : Natural
zero =
    Natural []


one : Natural
one =
    Natural [1]


two : Natural
two =
    Natural [2]


three : Natural
three =
    Natural [3]


four : Natural
four =
    Natural [4]


five : Natural
five =
    Natural [5]


six : Natural
six =
    Natural [6]


seven : Natural
seven =
    Natural [7]


eight : Natural
eight =
    Natural [8]


nine : Natural
nine =
    Natural [9]


ten : Natural
ten =
    Natural [10]


-- CONSTRUCT


fromInt : Int -> Maybe Natural
fromInt x =
    if x >= 0 && x <= maxSafeInteger then
        Just <| Natural <| fromIntHelper [] x

    else
        Nothing


maxBits : Int
maxBits =
    53


maxSafeInteger : Int
maxSafeInteger =
    2 ^ maxBits - 1


fromIntHelper : List Int -> Int -> List Int
fromIntHelper digitsBE n =
    if n == 0 then
        List.reverse digitsBE

    else
        let
            (q, r) =
                n |> quotientModBy base
        in
        fromIntHelper (r :: digitsBE) q


fromBinaryString : String -> Maybe Natural
fromBinaryString =
    -- Format: one or more binary digits.
    fromBaseBString 2


fromOctalString : String -> Maybe Natural
fromOctalString =
    -- Format: one or more octal digits.
    fromBaseBString 8


fromHexString : String -> Maybe Natural
fromHexString =
    -- Format: one or more hexadecimal digits.
    fromBaseBString 16


fromString : String -> Maybe Natural
fromString input =
    -- input   ::= ('0b' | '0B') binary
    --           | ('0o' | '0O') octal
    --           | ('0x' | '0X') hex
    --           | decimal
    -- binary  ::= [0-1]+
    -- octal   ::= [0-7]+
    -- hex     ::= [0-9a-fA-F]+
    -- decimal ::= [0-9]+
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


fromBaseBString : Int -> String -> Maybe Natural
fromBaseBString b input =
    if isBaseB b && isBaseBString b input then
        Just <| Natural <|
            String.foldl
                -- To satisfy the assumptions of sdAdd and sdMul
                -- we need base-1 >= b.
                (\char x -> sdAdd (sdMul x b 0 []) (toBaseBDigit b char) [])
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
    if (0x30 <= code && code <= Basics.min (0x30 + b - 1) 0x39) then
        code - 0x30

    else if (0x41 <= code && code <= 0x41 + b - 11) then
        code - 0x41 + 10

    else
        code - 0x61 + 10


-- COMPARISON


compare : Natural -> Natural -> Order
compare (Natural xsLE) (Natural ysLE) =
    let
        xsLen =
            List.length xsLE

        ysLen =
            List.length ysLE
    in
    if xsLen < ysLen then
        LT

    else if xsLen > ysLen then
        GT

    else
        compareHelper (List.reverse xsLE) (List.reverse ysLE)


compareHelper : List Int -> List Int -> Order
compareHelper xsBE ysBE =
    --
    -- Assumptions
    --
    -- 1. xsBE = [ x_n, ..., x_1, x_0 ] (BE) and 0 <= xi <= base-1
    -- 2. ysBE = [ y_n, ..., y_1, y_0 ] (BE) and 0 <= yi <= base-1
    --
    case (xsBE, ysBE) of
        (x :: restXsBE, y :: restYsBE) ->
            if x < y then
                LT

            else if x > y then
                GT

            else
                compareHelper restXsBE restYsBE

        _ ->
            EQ


isLessThan : Natural -> Natural -> Bool
isLessThan b a =
    -- Is a < b?
    compare a b == LT


isLessThanOrEqual : Natural -> Natural -> Bool
isLessThanOrEqual b a =
    -- Is a <= b?
    --
    -- a <= b iff not (a > b)
    not (a |> isGreaterThan b)


isGreaterThan : Natural -> Natural -> Bool
isGreaterThan b a =
    -- Is a > b?
    compare a b == GT


isGreaterThanOrEqual : Natural -> Natural -> Bool
isGreaterThanOrEqual b a =
    -- Is a >= b?
    --
    -- a >= b iff not (a < b)
    not (a |> isLessThan b)


max : Natural -> Natural -> Natural
max a b =
    if a |> isGreaterThanOrEqual b then
        a

    else
        b


min : Natural -> Natural -> Natural
min a b =
    if a |> isLessThanOrEqual b then
        a

    else
        b


-- CLASSIFICATION


isZero : Natural -> Bool
isZero (Natural digits) =
    digits == []


isNonZero : Natural -> Bool
isNonZero =
    not << isZero


isEven : Natural -> Bool
isEven (Natural digitsLE) =
    case digitsLE of
        [] ->
            True

        d :: _ ->
            modBy 2 d == 0


isOdd : Natural -> Bool
isOdd =
    not << isEven


-- ARITHMETIC


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
    case (xsLE, ysLE) of
        ([], []) ->
            let
                finalZsBE =
                    if carry == 0 then
                        zsBE

                    else
                        carry :: zsBE
            in
            List.reverse finalZsBE

        (x :: restXsLE, []) ->
            let
                (newCarry, z) =
                    x + carry |> iDivModBy base
            in
            addHelper restXsLE [] newCarry (z :: zsBE)

        ([], y :: restYsLE) ->
            let
                (newCarry, z) =
                    y + carry |> iDivModBy base
            in
            addHelper [] restYsLE newCarry (z :: zsBE)

        (x :: restXsLE, y :: restYsLE) ->
            let
                (newCarry, z) =
                    x + y + carry |> iDivModBy base
            in
            addHelper restXsLE restYsLE newCarry (z :: zsBE)


sub : Natural -> Natural -> Natural
sub (Natural xsLE) (Natural ysLE) =
    -- Saturating subtraction
    --
    -- x - y = n, where n + y = x and n is a natural number (if x >= y)
    --       = 0                                            (if x < y)
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
    case (xsLE, ysLE) of
        ([], []) ->
            if carry == 0 then
                zsBE
                    |> removeLeadingZeros
                    |> List.reverse

            else -- carry == -1 which means xsLE < ysLE
                []

        (x :: restXsLE, []) ->
            let
                (newCarry, z) =
                    x + carry |> quotientModBy base
            in
            subHelper restXsLE [] newCarry (z :: zsBE)

        ([], y :: restYsLE) ->
            let
                (newCarry, z) =
                    carry - y |> quotientModBy base
            in
            subHelper [] restYsLE newCarry (z :: zsBE)

        (x :: restXsLE, y :: restYsLE) ->
            let
                (newCarry, z) =
                    x - y + carry |> quotientModBy base
            in
            subHelper restXsLE restYsLE newCarry (z :: zsBE)


mul : Natural -> Natural -> Natural
mul (Natural xsLE) (Natural ysLE) =
    Natural <| mulHelper xsLE (List.reverse ysLE) []


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
                    sdMul xsLE y 0 []

                -- (base * zsLE) + (xsLE * y)
                partialSum =
                    addHelper augend addend 0 []
            in
            mulHelper xsLE restYsBE partialSum


divModBy : Natural -> Natural -> Maybe (Natural, Natural)
divModBy (Natural ysLE as y) (Natural xsLE as x) =
    case ysLE of
        [] ->
            Nothing

        [d] ->
            let
                (qs, r) =
                    sdDivMod xsLE d [] 0
            in
            Just
                ( Natural qs
                , if r == 0 then
                    zero

                  else
                    Natural [r]
                )

        _ ->
            case compare x y of
                LT ->
                    Just (zero, x)

                EQ ->
                    Just (one, zero)

                GT ->
                    let
                        twoY =
                            Natural <| sdMul ysLE 2 0 []
                    in
                    x
                        |> divModBy twoY
                        |> Maybe.map
                            (\(Natural qsLE, r) ->
                                let
                                    twoQsLE =
                                        sdMul qsLE 2 0 []
                                in
                                if r |> isLessThan y then
                                    ( Natural twoQsLE
                                    , r
                                    )

                                else
                                    ( Natural <| sdAdd twoQsLE 1 []
                                    , sub r y
                                    )
                            )


exp : Natural -> Natural -> Natural
exp b n =
    --
    -- Can we implement a tail-recursive version?
    --
    if isZero n then
        one

    else if isEven n then
        let
            (m, _) =
                -- Can we avoid division? Maybe use a bitwise right shift?
                -- Can we avoid computing the remainder?
                n |> divModBy two |> Maybe.withDefault (one, zero)

            power =
                exp b m
        in
        -- Is there a squaring algorithm that's more efficient?
        mul power power

    else
        -- Compute b * b^{n - 1}
        --
        -- Can we avoid subtracting 1?
        mul b <| exp b <| sub n one


-- CONVERT


toInt : Natural -> Int
toInt (Natural digitsLE) =
    case digitsLE of
        [] ->
            0

        _ ->
            let
                (q, r) =
                    iDivModBy numBits maxBits

                (len, maskStart) =
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


toBinaryString : Natural -> String
toBinaryString =
    toBaseBString 2 >> Maybe.withDefault ""


toOctalString : Natural -> String
toOctalString =
    toBaseBString 8 >> Maybe.withDefault ""


toHexString : Natural -> String
toHexString =
    toBaseBString 16 >> Maybe.withDefault ""


toString : Natural -> String
toString =
    toBaseBString 10 >> Maybe.withDefault ""


toBaseBString : Int -> Natural -> Maybe String
toBaseBString b (Natural xs) =
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
                (q, r) =
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
            0x61 + offset - 10


isBaseB : Int -> Bool
isBaseB b =
    2 <= b && b <= 36


-- SINGLE-DIGIT OPERATIONS


sdAdd : List Int -> Int -> List Int -> List Int
sdAdd xs y zs =
    --
    -- zs = xs + y
    --
    -- Assumptions
    --
    -- 1. xs = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. 0 <= y <= base-1
    -- 3. zs = [ z_m, ..., z_1, z_0 ] (BE) and 0 <= zj <= base-1
    --
    case xs of
        [] ->
            List.reverse <|
                if y == 0 then
                    zs

                else
                    y :: zs

        x :: restXs ->
            let
                (carry, z) =
                    iDivModBy base (x + y)
            in
            sdAdd restXs carry (z :: zs)


sdSub : List Int -> Int -> List Int -> List Int
sdSub xs y zs =
    --
    -- zs = xs - y
    --
    -- Assumptions
    --
    -- 1. xs = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. 0 <= y <= base-1
    -- 3. valueOf xs >= valueOf y
    -- 4. zs = [ z_m, ..., z_1, z_0 ] (BE) and 0 <= zj <= base-1
    --
    case xs of
        [] ->
            zs
                |> removeLeadingZeros
                |> List.reverse

        x :: restXs ->
            let
                diff =
                    x - y

                (carry, z) =
                    if diff >= 0 then
                        ( 0
                        , diff
                        )

                    else
                        ( 1
                        , diff + base
                        )

            in
            sdSub restXs carry (z :: zs)


sdMul : List Int -> Int -> Int -> List Int -> List Int
sdMul xs y carry zs =
    --
    -- zs = xs * y + carry
    --
    -- Assumptions
    --
    -- 1. xs = [ x_0, x_1, ..., x_n ] (LE) and 0 <= xi <= base-1
    -- 2. 0 <= y <= base-1
    -- 3. 0 <= carry <= base-2
    -- 4. zs = [ z_m, ..., z_1, z_0 ] (BE) and 0 <= zj <= base-1
    --
    case xs of
        [] ->
            List.reverse <|
                if carry == 0 then
                    zs

                else
                    carry :: zs

        x :: restXs ->
            let
                -- product constrains how large the base can be.
                --
                -- 0 <= product <= (base-1)(base-1) + base-2
                --              <= base^2 - base - 1
                --
                -- And, we want
                --
                -- base^2 - base - 1 <= maxSafeInteger
                --
                -- Since we want base to be a power of 2, i.e. base = 2^n, then
                --
                -- 2^{2n} - 2^n - 1 <= maxSafeInteger
                --                  <= 2^53 - 1
                --     2^{2n} - 2^n <= 2^53
                --
                -- This gives n <= 26, i.e. the maximum base we will be able to
                -- use is 2^26.
                product =
                    x * y + carry

                (newCarry, z) =
                    iDivModBy base product
            in
            sdMul restXs y newCarry (z :: zs)


sdDivMod : List Int -> Int -> List Int -> Int -> (List Int, Int)
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


sdDivModHelper : List Int -> Int -> Bool -> List Int -> Int -> (List Int, Int)
sdDivModHelper xs y isTrailingZero qs r =
    case xs of
        [] ->
            (qs, r)

        x :: restXs ->
            let
                -- value constrains how large the base can be.
                --
                -- 0 <= value <= (base-1)*base + base-1
                --            <= base^2 - 1
                --
                -- And, we want
                --
                -- base^2 - 1 <= maxSafeInteger
                --
                -- Since we want base to be a power of 2, i.e. base = 2^n, then
                --
                -- 2^{2n} - 1 <= maxSafeInteger
                --            <= 2^53 - 1
                --     2^{2n} <= 2^53
                --         2n <= 53
                --
                -- This gives n <= 26, i.e. the maximum base we will be able to
                -- use is 2^26.
                value =
                    r * base + x

                (q, newR) =
                    iDivModBy y value
            in
            if isTrailingZero && q == 0 then
                sdDivModHelper restXs y isTrailingZero qs newR

            else
                sdDivModHelper restXs y False (q :: qs) newR


-- HELPERS


iDivModBy : Int -> Int -> (Int, Int)
iDivModBy divisor dividend =
    ( dividend // divisor
    , modBy divisor dividend
    )


quotientModBy : Int -> Int -> (Int, Int)
quotientModBy divisor dividend =
    ( floor (toFloat dividend / toFloat divisor)
    , modBy divisor dividend
    )


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
