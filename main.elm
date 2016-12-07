module Main exposing (..)

import Array exposing (Array)
import Css
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import List.Split
import Regex exposing (Regex, Match)
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model, view = myView, update = myUpdate }


rainbowList : List (List String)
rainbowList =
    [ [ "Aqua", "Blue", "Green", "DarkTurquoise", "Fuschia", "Lime", "Plum" ], [ "Beige", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ] ]


type alias ColoredWord =
    { colors : Set String
    , text : String
    , normalized : String
    }


nonMaybeColoredWord : Maybe ColoredWord -> ColoredWord
nonMaybeColoredWord =
    Maybe.withDefault { text = "", colors = Set.empty, normalized = "" }


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    , workingWord : Int
    , hideColors : Set String
    , wordsPerLine : Int
    }


model : Model
model =
    { text = "Hello"
    , workingColor = ""
    , words = Array.fromList []
    , workingWord = -1
    , hideColors = Set.empty
    , wordsPerLine = 10
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
    | SetWordsPerLine String


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    let
        chunkArray : Array String
        chunkArray =
            Array.fromList (Regex.split Regex.All (Regex.regex "\\s+") input)

        chunkToColoredword : String -> ColoredWord
        chunkToColoredword str =
            let
                textAndColors : Maybe (List (Maybe String))
                textAndColors =
                    Regex.find Regex.All (Regex.regex "^([^<>]+)<([^>]+)>\\s*$") str
                        |> List.head
                        |> Maybe.map .submatches

                theTextMap : String
                theTextMap =
                    textAndColors
                        |> Maybe.andThen List.head
                        |> Maybe.withDefault Nothing
                        |> Maybe.withDefault str

                theColors : Set String
                theColors =
                    textAndColors
                        |> Maybe.map (List.drop 1)
                        |> Maybe.andThen List.head
                        |> Maybe.withDefault Nothing
                        |> Maybe.map (Regex.split Regex.All (Regex.regex ","))
                        |> Maybe.map Set.fromList
                        |> Maybe.withDefault Set.empty
            in
                { text = theTextMap
                , colors = theColors
                , normalized = normalize theTextMap
                }
    in
        Array.map chunkToColoredword chunkArray


normalize : String -> String
normalize text =
    Regex.replace Regex.All
        (Regex.regex "[^a-z0-9]")
        (\_ -> "")
        (String.toLower text)


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

        SetWordsPerLine wordString ->
            case
                String.toInt wordString
            of
                Err msg ->
                    model

                Ok val ->
                    { model | wordsPerLine = val }


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        (Set.remove element set)
    else
        (Set.insert element set)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style [ ( "backgroundColor", colorName ) ]


matchingWordsForColor : String -> Array ColoredWord -> List String
matchingWordsForColor color coloredWordList =
    List.filterMap
        (\cw ->
            if (Set.member color cw.colors) then
                Just cw.text
            else
                Nothing
        )
        (Array.toList coloredWordList)


countWordsMatching : Model -> Int
countWordsMatching model =
    let
        desired =
            (currentWordFromIndex model).normalized
    in
        Array.length (Array.filter (\cw -> (cw.normalized == desired)) model.words)


countWords : Model -> Int
countWords model =
    Array.length model.words


colorStyles : Set String -> ColoredWord -> ColoredWord -> Html.Attribute msg
colorStyles excludeSet coloredWord currentWord =
    let
        colorSet =
            Set.diff coloredWord.colors excludeSet

        size =
            Set.size colorSet

        matchingStyle =
            if (coloredWord.normalized == currentWord.normalized) then
                [ ( "borderStyle", "solid" ), ( "borderColor", "black" ) ]
            else
                [ ( "borderStyle", "solid" ), ( "borderColor", "transparent" ) ]
    in
        if (size == 0) then
            style matchingStyle
        else if (size <= 1) then
            style (( "backgroundColor", String.join "," (Set.toList colorSet) ) :: matchingStyle)
        else
            style (( "background", "linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) :: matchingStyle)


currentWordFromIndex : Model -> ColoredWord
currentWordFromIndex model =
    nonMaybeColoredWord (Array.get model.workingWord model.words)


enableButton : List String -> Html Msg
enableButton cs =
    button [ onClick (HideSomeColors cs) ] [ text "hide" ]


disableButton : List String -> Html Msg
disableButton cs =
    button [ onClick (ResetSomeColors cs) ] [ text "reset" ]


dumpColoredWord : ColoredWord -> String
dumpColoredWord cw =
    if (Set.isEmpty cw.colors) then
        cw.text
    else
        cw.text ++ "<" ++ (String.join "," (Set.toList cw.colors)) ++ ">"


dumpState : Model -> String
dumpState model =
    List.map dumpColoredWord (Array.toList model.words)
        |> (String.join " ")


type Id
    = MyId


type Class
    = MyClass


imports : List String
imports =
    []


rules : List { descriptor : Css.Descriptor, selectors : List (Css.Sel Id Class) }
rules =
    [ { selectors = [ Css.Class MyClass ]
      , descriptor = [ ( "counter-increment", "line" ) ]
      }
    , { selectors = [ Css.Pseudo [ Css.Before ] (Css.Class MyClass) ]
      , descriptor = [ ( "content", "counter(line)" ), ( "color", "red" ) ]
      }
    ]


stylesheet : Css.Stylesheet Id Class msg
stylesheet =
    Css.stylesheet imports rules


myView : Model -> Html Msg
myView model =
    div []
        [ Css.style [ Html.Attributes.scoped True ] stylesheet
        , span
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
        , Html.p
            []
            [ button [ onClick EnableAllColors ] [ text "ResetHiding" ]
            , input [ value (toString model.wordsPerLine), onInput SetWordsPerLine ] [ text "WordsPerLine" ]
            , span []
                [ text
                    (toString (countWordsMatching model)
                        ++ "/"
                        ++ (toString (countWords model))
                    )
                ]
            ]
        , input
            [ value (String.join ", " (matchingWordsForColor model.workingColor model.words))
            , style [ ( "width", "800px" ) ]
            , readonly True
            , colorStyle model.workingColor
            ]
            []
        , Html.p
            []
            (let
                indexedList : List ( Int, ColoredWord )
                indexedList =
                    (Array.toIndexedList model.words)

                partitionedList : List (List ( Int, ColoredWord ))
                partitionedList =
                    List.Split.chunksOfLeft model.wordsPerLine indexedList

                wordToMatch : ColoredWord
                wordToMatch =
                    currentWordFromIndex model

                renderWord : ( Int, ColoredWord ) -> Html Msg
                renderWord ( index, w ) =
                    span
                        [ colorStyles model.hideColors w wordToMatch
                        , onClick (ToggleColor index model.workingColor)
                        , onMouseEnter (SetCurrentWord index)
                        ]
                        [ text (" " ++ w.text ++ " ") ]

                renderWords : List ( Int, ColoredWord ) -> List (Html Msg)
                renderWords indexedList =
                    List.map renderWord indexedList

                renderLine : List ( Int, ColoredWord ) -> Html Msg
                renderLine listPart =
                    Html.div [ stylesheet.class MyClass ] (renderWords listPart)

                renderPartitioned : List (List ( Int, ColoredWord )) -> List (Html Msg)
                renderPartitioned partitionedList =
                    List.map renderLine partitionedList
             in
                -- renderWords indexedList
                renderPartitioned partitionedList
            )
        ]
