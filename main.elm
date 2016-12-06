module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Regex exposing (Regex, Match)
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = myView, update = myUpdate }


rainbowList : List (List String)
rainbowList =
    [ [ "Blue", "Green", "DarkTurquoise" ], [ "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ] ]


type alias ColoredWord =
    { colors : Set String
    , text : String
    }


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    , workingWord : Int
    , hideColors : Set String
    }


model : Model
model =
    { text = "Hello"
    , workingColor = ""
    , words = Array.fromList []
    , workingWord = -1
    , hideColors = Set.empty
    }


type Msg
    = SetText String
    | SetCurrentColor String
    | ToggleColor Int String
    | SetCurrentWord Int
    | ToggleColorEnabled String
    | EnableAllColors
    | HideSomeColors (List String)
    | ResetSomeColors (List String)


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    let
        chunkArray : Array String
        chunkArray =
            Array.fromList (Regex.split Regex.All (Regex.regex "\\s+") input)

        chunkToColoredword : String -> ColoredWord
        chunkToColoredword str =
            let
                textAndColors : List Match
                textAndColors =
                    Regex.find Regex.All (Regex.regex "^([^<>]+)<([^>]+)>\\s*$") str

                theTextMap : String
                theTextMap =
                    Maybe.withDefault str
                        (Maybe.withDefault Nothing
                            (Maybe.andThen List.head
                                (Maybe.map .submatches (List.head textAndColors))
                            )
                        )

                theText : String
                theText =
                    case (List.head textAndColors) of
                        Nothing ->
                            str

                        Just match ->
                            case (List.head match.submatches) of
                                Nothing ->
                                    str

                                Just maybeText ->
                                    case maybeText of
                                        Nothing ->
                                            str

                                        Just text ->
                                            text

                theColors : Set String
                theColors =
                    case (List.head textAndColors) of
                        Nothing ->
                            Set.empty

                        Just match ->
                            case (List.head (List.drop 1 match.submatches)) of
                                Nothing ->
                                    Set.empty

                                Just maybeString ->
                                    case maybeString of
                                        Nothing ->
                                            Set.empty

                                        Just s ->
                                            Set.fromList (Regex.split Regex.All (Regex.regex ",") s)
            in
                { text = theTextMap
                , colors = theColors
                }
    in
        Array.map chunkToColoredword chunkArray


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
            }

        SetCurrentColor newDefaultColor ->
            { model | workingColor = newDefaultColor }

        SetCurrentWord index ->
            { model | workingWord = index }

        ToggleColorEnabled color ->
            { model | hideColors = toggleSet color model.hideColors }

        ToggleColor which newColor ->
            if (String.length newColor == 0) then
                model
            else
                let
                    currentColoredWord =
                        nonMaybeColoredWord (Array.get which model.words)

                    modifiedColoredWord =
                        { currentColoredWord | colors = toggleSet newColor currentColoredWord.colors }
                in
                    { model | words = Array.set which modifiedColoredWord model.words }

        EnableAllColors ->
            { model | hideColors = Set.empty }

        HideSomeColors colorList ->
            { model | hideColors = Set.union model.hideColors (Set.fromList colorList) }

        ResetSomeColors colorList ->
            { model | hideColors = Set.diff model.hideColors (Set.fromList colorList) }


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet a s =
    if (Set.member a s) then
        (Set.remove a s)
    else
        (Set.insert a s)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style [ ( "backgroundColor", colorName ) ]


matchingWordsForColor : String -> Array ColoredWord -> List String
matchingWordsForColor color coloredWordList =
    let
        fw : ColoredWord -> Maybe String
        fw cw =
            if (Set.member color cw.colors) then
                Just cw.text
            else
                Nothing
    in
        List.filterMap fw (Array.toList coloredWordList)


colorStyles : Set String -> ColoredWord -> ColoredWord -> Html.Attribute msg
colorStyles excludeSet coloredWord currentWord =
    let
        colorSet =
            Set.diff coloredWord.colors excludeSet

        size =
            Set.size colorSet

        matchingStyle =
            if (coloredWord.text == currentWord.text) then
                [ ( "borderStyle", "solid" ), ( "borderColor", "black" ) ]
            else
                [ ( "borderStyle", "solid" ), ( "borderColor", "transparent" ) ]
    in
        if (size == 0) then
            style matchingStyle
        else if (size <= 1) then
            style (( "backgroundColor", String.join "," (Set.toList colorSet) ) :: matchingStyle)
        else
            style (( "background", "repeating-linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) :: matchingStyle)


currentWordFromIndex : Model -> ColoredWord
currentWordFromIndex model =
    nonMaybeColoredWord (Array.get model.workingWord model.words)


enableButton : List String -> Html Msg
enableButton cs =
    button [ onClick (HideSomeColors cs) ] [ text "hide" ]


disableButton : List String -> Html Msg
disableButton cs =
    button [ onClick (ResetSomeColors cs) ] [ text "reset" ]


dumpState : Model -> String
dumpState model =
    let
        f cw =
            if (Set.isEmpty cw.colors) then
                cw.text
            else
                cw.text ++ "<" ++ (String.join "," (Set.toList cw.colors)) ++ ">"
    in
        String.join " "
            (List.map f (Array.toList model.words))


myView : Model -> Html Msg
myView model =
    div []
        [ span
            []
            [ text (toString (Set.toList ((currentWordFromIndex model).colors))) ]
        , p [] []
        , input
            [ value (dumpState model)
            , onInput SetText
            , style [ ( "width", "800px" ) ]
            ]
            []
        , div
            []
            (let
                doCell : String -> Html Msg
                doCell l =
                    td []
                        [ input
                            [ type_ "checkbox"
                            , onClick (ToggleColorEnabled l)
                            , checked (Set.member l model.hideColors)
                            ]
                            []
                        , button [ colorStyle l, onClick (SetCurrentColor l) ] [ text l ]
                        ]

                doRow : List String -> Html Msg
                doRow ls =
                    table [] [ tr [] (enableButton ls :: disableButton ls :: (List.map doCell ls)) ]
             in
                List.map doRow rainbowList
            )
        , Html.p [] [ button [ onClick EnableAllColors ] [ text "ResetHiding" ] ]
        , input
            [ value (String.join ", " (matchingWordsForColor model.workingColor model.words))
            , style [ ( "width", "800px" ) ]
            , readonly True
            , colorStyle model.workingColor
            ]
            []
        , Html.p []
            (List.map
                (\( index, w ) ->
                    span
                        [ colorStyles model.hideColors w (currentWordFromIndex model)
                        , onClick (ToggleColor index model.workingColor)
                        , onMouseEnter (SetCurrentWord index)
                        ]
                        [ text (" " ++ w.text ++ " ") ]
                )
                (Array.toIndexedList model.words)
            )
        ]
