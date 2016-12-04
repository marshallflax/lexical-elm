module Main exposing (..)

import Html exposing (Html, button, div, span, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (..)
import Array exposing (Array)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = myView, update = myUpdate }


rainbowList : List String
rainbowList =
    [ "Blue", "Green", "DarkTurquoise", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ]


type alias ColoredWord =
    { text : String, color : String }


type alias Model =
    { counter : Int
    , text : String
    , workingColor : String
    , words : Array ColoredWord
    }


model : Model
model =
    { counter = 0, text = "Hello", workingColor = "", words = Array.fromList [] }


type Msg
    = More
    | Less
    | SetText String
    | SetCurrentColor String


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    Array.map (\w -> { text = w, color = "" })
        (Array.fromList
            (Regex.split Regex.All (Regex.regex "\\s+") input)
        )


myUpdate : Msg -> Model -> Model
myUpdate msg model =
    case msg of
        More ->
            { model | counter = model.counter + 1 }

        Less ->
            { model | counter = model.counter - 1 }

        SetText newtext ->
            { model | text = newtext, words = splitIntoColorwords newtext }

        SetCurrentColor newDefaultColor ->
            { model | workingColor = newDefaultColor }


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style
        [ ( "backgroundColor", colorName )
        , ( "fontFamily", "Calibri,serif" )
        ]


myView : Model -> Html Msg
myView model =
    div []
        [ div []
            [ button [ onClick Less ] [ text "-" ]
            , div [] [ text (toString model.counter) ]
            , button [ onClick More ] [ text "+" ]
            ]
        , span [ colorStyle model.workingColor ]
            [ text model.text ]
        , input
            [ placeholder "Text to reverse"
            , onInput SetText
            ]
            []
        , div
            [ colorStyle model.workingColor ]
            (List.map
                (\l ->
                    button
                        [ colorStyle l, onClick (SetCurrentColor l) ]
                        [ text l ]
                )
                rainbowList
            )
        , Html.p []
            (List.map (\(index, w) -> span [ colorStyle w.text ] [ text ("{" ++ w.text ++ "}") ])
                (Array.toIndexedList model.words)
            )
        ]
