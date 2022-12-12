module Natural exposing
    ( Natural
    , zero, one, two, ten
    , fromInt
    , toInt
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


-- MISC


divModBy : Int -> Int -> (Int, Int)
divModBy divisor dividend =
    ( dividend // divisor
    , modBy divisor dividend
    )


padLeft : Int -> a -> List a -> List a
padLeft n x list =
    List.repeat (n - List.length list) x ++ list
