module E exposing (main)

import Html as H
import Lib


main : H.Html msg
main =
    H.p
        []
        [ H.text "The first 100 digits of e is "
        , H.strong [] [ H.text <| Lib.firstNDigitsOfE 100 ]
        , H.text "."
        ]
