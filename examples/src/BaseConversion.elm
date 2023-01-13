module BaseConversion exposing (main)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Natural


main : Program () Model Msg
main =
    B.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { input : String
    }


init : Model
init =
    { input = ""
    }



-- UPDATE


type Msg
    = ChangedInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedInput newInput ->
            { model | input = newInput }



-- VIEW


view : Model -> H.Html Msg
view { input } =
    H.div []
        [ viewField input
        , let
            maybeN =
                input
                    |> String.trim
                    |> Natural.fromBaseBString 10
          in
          case maybeN of
            Just n ->
                H.div []
                    [ H.p []
                        [ H.text <| "Binary: " ++ Natural.toBinaryString n ]
                    , H.p []
                        [ H.text <| "Octal: " ++ Natural.toOctalString n ]
                    , H.p []
                        [ H.text <| "Hexadecimal: " ++ Natural.toHexString n ]
                    ]

            Nothing ->
                H.text ""
        ]


viewField : String -> H.Html Msg
viewField input =
    H.div []
        [ H.p [] [ H.text "Please enter a non-negative integer (base 10):" ]
        , H.p []
            [ H.input
                [ HA.autofocus True
                , HA.placeholder "For e.g. 0, 1, 2, ..., 1729, ..."
                , HA.value input
                , HE.onInput ChangedInput
                ]
                []
            ]
        ]
