module Lib exposing
    ( divModBy, quotientModBy
    , padLeft
    , removeLeadingZeros
    )


divModBy : Int -> Int -> (Int, Int)
divModBy divisor dividend =
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
