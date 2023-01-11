module Factorial exposing (main)

import Html as H
import Natural exposing (Natural)


main : H.Html msg
main =
    List.range 0 100
        |> List.map (viewFact << fromInt)
        |> H.div []


fromInt : Int -> Natural
fromInt =
    Natural.fromInt >> Maybe.withDefault Natural.zero


viewFact : Natural -> H.Html msg
viewFact n =
    H.div []
        [ H.text <|
            Natural.toString n
                ++ "! = "
                ++ Natural.toString (fact n)
        ]


fact : Natural -> Natural
fact n =
    if Natural.isZero n then
        Natural.one

    else
        Natural.mul n (fact (Natural.sub n Natural.one))
