module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, button, div, span, text, input, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex exposing (..)
import Set exposing (Set)
import HtmlParser as HtmlParser exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = myView, update = myUpdate }


rainbowList : List String
rainbowList =
    [ "Blue", "Green", "DarkTurquoise", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ]


type alias ColoredWord =
    { colors : Set String
    , text : String
    }


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    , workingWord : Int
    , parsed : List Node
    }


model : Model
model =
    { text = "Hello"
    , workingColor = ""
    , words = Array.fromList []
    , workingWord = -1
    , parsed = []
    }


type Msg
    = SetText String
    | SetCurrentColor String
    | ToggleColor Int String
    | SetCurrentWord Int


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    Array.map (\w -> { text = w, colors = Set.empty })
        (Array.fromList
            (Regex.split Regex.All (Regex.regex "\\s+") input)
        )


nonMaybeColoredWord : Maybe ColoredWord -> ColoredWord
nonMaybeColoredWord x =
    Maybe.withDefault { text = "", colors = Set.empty } x


myUpdate : Msg -> Model -> Model
myUpdate msg model =
    case msg of
        SetText newtext ->
            { model
                | text = newtext
                , words = splitIntoColorwords newtext
                , parsed = HtmlParser.parse newtext
            }

        SetCurrentColor newDefaultColor ->
            { model | workingColor = newDefaultColor }

        SetCurrentWord index ->
            { model | workingWord = index }

        ToggleColor which newColor ->
            if (String.length newColor == 0) then
                model
            else
                let
                    currentColoredWord =
                        nonMaybeColoredWord (Array.get which model.words)

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
    style [ ( "backgroundColor", colorName ) ]


colorStyles : ColoredWord -> ColoredWord -> Html.Attribute msg
colorStyles coloredWord currentWord =
    let
        colorNameSet =
            coloredWord.colors

        size =
            Set.size colorNameSet

        matchingStyle =
            if (coloredWord.text == currentWord.text) then
                [ ( "borderStyle", "solid" ) ]
            else
                []
    in
        if (size == 0) then
            style matchingStyle
        else
            let
                list =
                    String.join "," (Set.toList colorNameSet)
            in
                if (size <= 1) then
                    style (( "backgroundColor", list ) :: matchingStyle)
                else
                    style (( "background", "repeating-radial-gradient(" ++ list ++ ")" ) :: matchingStyle)


currentWordFromIndex : Model -> ColoredWord
currentWordFromIndex model =
    nonMaybeColoredWord (Array.get model.workingWord model.words)


myView : Model -> Html Msg
myView model =
    div []
        [ span
            [ colorStyle model.workingColor ]
            [ text (toString (currentWordFromIndex model)) ]
        , p [] [ text (toString model.parsed) ]
        , input
            [ placeholder "Text to reverse", onInput SetText ]
            []
        , div
            [ colorStyle model.workingColor ]
            (List.map
                (\l ->
                    button
                        [ colorStyle l
                        , onClick (SetCurrentColor l)
                        ]
                        [ text l ]
                )
                rainbowList
            )
        , Html.p []
            (List.map
                (\( index, w ) ->
                    span
                        [ colorStyles w (currentWordFromIndex model)
                        , onClick (ToggleColor index model.workingColor)
                        , onMouseEnter (SetCurrentWord index)
                        ]
                        [ text (" " ++ w.text ++ " ")]
                )
                (Array.toIndexedList model.words)
            )
        ]
