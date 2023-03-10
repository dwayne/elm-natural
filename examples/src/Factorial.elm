module Factorial exposing (main)

import Html as H
import Lib
import Natural as N


main : H.Html msg
main =
    List.range 0 100
        |> List.map viewFact
        |> H.div []


viewFact : Int -> H.Html msg
viewFact n =
    H.div []
        [ H.text <|
            String.fromInt n
                ++ "! = "
                ++ N.toString (Lib.fact (N.fromSafeInt n))
        ]
