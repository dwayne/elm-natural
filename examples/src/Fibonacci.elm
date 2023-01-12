module Fibonacci exposing (main)

import Html as H
import Lib
import Natural


main : H.Html msg
main =
    List.range 0 999
        |> List.map viewFib
        |> H.div []


viewFib : Int -> H.Html msg
viewFib n =
    let
        lhs =
            if n == 0 || n == 1 then
                viewFSubN n

            else
                H.span []
                    [ viewFSubN n
                    , H.text " = "
                    , viewFSubN <| n - 1
                    , H.text " + "
                    , viewFSubN <| n - 2
                    ]

        rhs =
            Lib.fib n
                |> Natural.toString
                |> H.text
    in
    H.div [] [ lhs, H.text " = ", rhs ]


viewFSubN : Int -> H.Html msg
viewFSubN n =
    H.span []
        [ H.text "F"
        , H.sub [] [ H.text <| String.fromInt n ]
        ]
