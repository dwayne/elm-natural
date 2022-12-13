module Natural exposing
    ( Natural
    , zero, one, two, ten
    , fromInt
    , toInt

    -- For testing purposes
    , sdAdd
    )


import Bitwise


-- REPRESENTATION


numBits : Int
numBits =
    4


base : Int
base =
    2 ^ numBits


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


-- MISC


divModBy : Int -> Int -> (Int, Int)
divModBy divisor dividend =
    ( dividend // divisor
    , modBy divisor dividend
    )


padLeft : Int -> a -> List a -> List a
padLeft n x list =
    List.repeat (n - List.length list) x ++ list
