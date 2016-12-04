module Main exposing (..)

import Html exposing (Html, button, div, span, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (..)


rainbowList : List String
rainbowList =
    [ "Blue", "Green", "DarkTurquoise", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ]


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style
        [ ( "backgroundColor", colorName )
        , ( "fontFamily", "Calibri,serif" )
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = myView, update = myUpdate }


type alias ColoredWord =
    { text : String, color : String }


type alias Model =
    { counter : Int
    , text : String
    , workingColor : String
    , words : List ColoredWord
    }


model : Model
model =
    { counter = 0, text = "Hello", workingColor = "", words = [] }


type Msg
    = More
    | Less
    | SetText String
    | SetCurrentColor String


splitIntoColorwords : String -> List ColoredWord
splitIntoColorwords input =
    List.map (\w -> { text = w, color = "" })
        (Regex.split Regex.All (Regex.regex "\\s+") input)


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
            (List.map (\w -> span [ colorStyle w.text ] [ text ("{" ++ w.text ++ "}") ])
                model.words
            )
        ]
