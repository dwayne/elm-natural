module Pi exposing (main)

import Html as H
import Lib


main : H.Html msg
main =
    H.p
        []
        [ H.text "The first 100 digits of π is "
        , H.strong [] [ H.text <| Lib.firstNDigitsOfPi 100 ]
        , H.text "."
        ]
