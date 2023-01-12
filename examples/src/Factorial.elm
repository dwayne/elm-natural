module Factorial exposing (main)

import Html as H
import Lib
import Natural exposing (Natural)


main : H.Html msg
main =
    List.range 0 100
        |> List.map (viewFact << Lib.fromInt)
        |> H.div []


viewFact : Natural -> H.Html msg
viewFact n =
    H.div []
        [ H.text <|
            Natural.toString n
                ++ "! = "
                ++ Natural.toString (Lib.fact n)
        ]
