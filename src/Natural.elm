module Natural exposing
    ( Natural
    , zero, one, two, ten
    , fromInt, fromBaseBString
    , toInt

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
            q =
                floor (toFloat n / toFloat base)

            r =
                modBy base n
        in
        fromIntHelper (r :: digitsBE) q


fromBaseBString : Int -> String -> Maybe Natural
fromBaseBString b input =
    if b >= 2 && b <= 36 && isBaseBString b input then
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
    -- 1. 2 <= b <= 36
    --
    input /= "" && String.all (isBaseBChar b) input


isBaseBChar : Int -> Char -> Bool
isBaseBChar b char =
    --
    -- Assumptions
    --
    -- 1. 2 <= b <= 36
    --
    let
        code =
            Char.toCode char
    in
    (0x30 <= code && code <= min (0x30 + b - 1) 0x39)
    || (0x41 <= code && code <= 0x41 + b - 11)
    || (0x61 <= code && code <= 0x61 + b - 11)


toBaseBDigit : Int -> Char -> Int
toBaseBDigit b char =
    --
    -- Assumptions
    --
    -- 1. 2 <= b <= 36
    -- 2. isBaseBChar b char == True
    --
    let
        code =
            Char.toCode char
    in
    if (0x30 <= code && code <= min (0x30 + b - 1) 0x39) then
        code - 0x30

    else if (0x41 <= code && code <= 0x41 + b - 11) then
        code - 0x41 + 10

    else
        code - 0x61 + 10


-- CONVERT


toInt : Natural -> Int
toInt (Natural digitsLE) =
    case digitsLE of
        [] ->
            0

        _ ->
            let
                (q, r) =
                    divModBy numBits maxBits

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
                    divModBy base (x + y)
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
                    divModBy base product
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
                    divModBy y value
            in
            if isTrailingZero && q == 0 then
                sdDivModHelper restXs y isTrailingZero qs newR

            else
                sdDivModHelper restXs y False (q :: qs) newR


-- HELPERS


divModBy : Int -> Int -> (Int, Int)
divModBy divisor dividend =
    ( dividend // divisor
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
