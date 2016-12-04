module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, button, div, span, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (..)
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = myView, update = myUpdate }


rainbowList : List String
rainbowList =
    [ "Blue", "Green", "DarkTurquoise", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ]


type alias ColoredWord =
    { text : String, colors : Set String }


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    }


model : Model
model =
    { text = "Hello"
    , workingColor = ""
    , words = Array.fromList []
    }


type Msg
    = SetText String
    | SetCurrentColor String
    | ToggleColor Int String


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    Array.map (\w -> { text = w, colors = Set.empty })
        (Array.fromList
            (Regex.split Regex.All (Regex.regex "\\s+") input)
        )


myUpdate : Msg -> Model -> Model
myUpdate msg model =
    case msg of
        SetText newtext ->
            { model | text = newtext, words = splitIntoColorwords newtext }

        SetCurrentColor newDefaultColor ->
            { model | workingColor = newDefaultColor }

        ToggleColor which newColor ->
            if (String.length newColor == 0) then
                model
            else
                let
                    currentColoredWord =
                        Maybe.withDefault { text = "", colors = Set.empty }
                            (Array.get which model.words)

                    currentColors =
                        currentColoredWord.colors

                    modifiedColors =
                        if (Set.member newColor currentColors) then
                            (Set.remove newColor currentColors)
                        else
                            (Set.insert newColor currentColors)

                    modifiedColoredWord =
                        { currentColoredWord | colors = modifiedColors }
                in
                    { model | words = Array.set which modifiedColoredWord model.words }


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style
        [ ( "backgroundColor", colorName )
        , ( "fontFamily", "Calibri,serif" )
        ]


colorStyles : Set String -> Html.Attribute msg
colorStyles colorNameSet =
    let
        size =
            Set.size colorNameSet

        list =
            String.join "," (Set.toList colorNameSet)
    in
        if (size <= 1) then
            style [ ( "backgroundColor", list ) ]
        else
            style [ ( "background", "repeating-radial-gradient(" ++ list ++ ")" ) ]


myView : Model -> Html Msg
myView model =
    div []
        [ span
            [ colorStyle model.workingColor ]
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
            (List.map
                (\( index, w ) ->
                    button
                        [ colorStyles w.colors
                        , onClick (ToggleColor index model.workingColor)
                        ]
                        [ text w.text ]
                )
                (Array.toIndexedList model.words)
            )
        ]
