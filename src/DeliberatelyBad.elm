module Main exposing (..)

import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)

main =
    beginnerProgram { model = 0, view = view, update = update }

type Msg
    = Transform (Int -> Int)

view model =
    div []
        [ button [ onClick (Transform ((+) -1)) ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick (Transform ((+) 1)) ] [ text "+" ]
        ]

update msg model =
    case msg of
        Transform xform ->
            xform model
