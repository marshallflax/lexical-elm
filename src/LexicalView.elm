module LexicalView exposing (viewLexicalModel, stylesheet)

import Array
import ColoredWord exposing (ColoredWord, matchingWordsForColor)
import ColoredWordView exposing (colorStyle, renderWord)
import Css
import Dict exposing (Dict)
import DragView exposing (viewDraggable)
import FreqInfo
import FreqInfoView
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import LexicalController exposing (countWords, countWordsMatching, currentWordFromIndex, dumpState, partitionedList, rainbowList)
import Misc
import MiscView exposing (onShiftedMouseEnter)
import Set exposing (Set)
import Types exposing (..)


viewLexicalModel : ( DraggableModel, LexicalModel ) -> Html Msg
viewLexicalModel ( draggables, lexicalModel ) =
    div []
        [ showColorsOfCurrentWord lexicalModel
        , p [] [ Misc.zipLists rainbowList |> toString |> text ]
        , showSaveButton
        , showTextInput lexicalModel
        , colorButtons draggables lexicalModel.hideColors
        , p [] []
        , viewDraggable draggables "text1" resetButtons
        , wordsPerLine lexicalModel
        , wordStats lexicalModel
        , wordsForColor lexicalModel
        , frequencyStats lexicalModel
        ]


showColorsOfCurrentWord : LexicalModel -> Html Msg
showColorsOfCurrentWord lexicalModel =
    let
        currentWord =
            currentWordFromIndex lexicalModel.workingWord lexicalModel.words
    in
        span []
            [ currentWord.colors |> Set.toList |> toString |> text ]


showSaveButton : Html Msg
showSaveButton =
    button [ onClick SaveModel ] [ text "Save" ]


showTextInput : LexicalModel -> Html Msg
showTextInput lexicalModel =
    input
        [ value (dumpState lexicalModel)
        , onInput (LexicalMessage << SetText)
        , style [ ( "width", "800px" ) ]
        ]
        []


colorButtons : DraggableModel -> Set String -> Html Msg
colorButtons draggablesModel hideColors =
    div
        []
        (let
            enableButton : List String -> Html Msg
            enableButton cs =
                button [ onClick (LexicalMessage (HideSomeColors cs)) ] [ text "hide" ]

            disableButton : List String -> Html Msg
            disableButton cs =
                button [ onClick (LexicalMessage (ResetSomeColors cs)) ] [ text "reset" ]

            doCell : String -> Html Msg
            doCell l =
                viewDraggable draggablesModel
                    l
                    (td []
                        [ input
                            [ type_ "checkbox"
                            , onClick (LexicalMessage (ToggleColorEnabled l))
                            , checked (Set.member l hideColors)
                            ]
                            []
                        , button
                            [ Html.Attributes.attribute "id" ("colorButton" ++ l)
                            , colorStyle l
                            , onClick (LexicalMessage (SetCurrentColor l))
                            ]
                            [ text l ]
                        ]
                    )

            doRow : List String -> Html Msg
            doRow ls =
                table [] [ tr [] (enableButton ls :: disableButton ls :: (List.map doCell ls)) ]
         in
            List.map doRow rainbowList
        )


resetButtons : Html Msg
resetButtons =
    button [ onClick (LexicalMessage EnableAllColors) ] [ text "ResetHiding" ]


wordsPerLine : LexicalModel -> Html Msg
wordsPerLine lexicalModel =
    input
        [ value (toString lexicalModel.wordsPerLine)
        , onInput (LexicalMessage << SetWordsPerLine)
        ]
        [ text "WordsPerLine" ]


wordStats : LexicalModel -> Html Msg
wordStats lexicalModel =
    span []
        [ text (toString (countWordsMatching lexicalModel) ++ "/" ++ (toString (countWords lexicalModel))) ]


wordsForColor : LexicalModel -> Html Msg
wordsForColor lexicalModel =
    input
        [ value
            (matchingWordsForColor lexicalModel.workingColor lexicalModel.words
                |> Array.toList
                |> String.join ", "
            )
        , style [ ( "width", "800px" ) ]
        , readonly True
        , colorStyle lexicalModel.workingColor
        ]
        []


cellStyle : String -> Html.Attribute Msg
cellStyle width =
    style [ ( "width", width ), ( "vertical-align", "top" ) ]


renderWords : LexicalModel -> List (Html Msg)
renderWords lexicalModel =
    let
        doWord : ( Int, ColoredWord ) -> Html Msg
        doWord =
            renderWord lexicalModel.hideColors lexicalModel.workingColor lexicalModel.workingNormalized

        renderLine : List ( Int, ColoredWord ) -> Html Msg
        renderLine listPart =
            Html.div [ stylesheet.class MyClass ] (List.map doWord listPart)
    in
        List.map renderLine (partitionedList lexicalModel)


renderGraphs : LexicalModel -> List (Html Msg)
renderGraphs lexicalModel =
    let
        chars : List String
        chars =
            String.toList lexicalModel.graphs |> List.map String.fromChar

        charAndTriplet : List ( String, String )
        charAndTriplet =
            Misc.zipShifts " " (List.range 0 2) chars
                |> Misc.zipLists
                |> List.map String.concat
                |> List.map2 (,) chars

        lenInfo : Dict String Int
        lenInfo =
            Dict.get 3 lexicalModel.ngraphs
                |> Maybe.withDefault FreqInfo.emptyLenInfo
                |> .tokenToCount

        numberToColor : Int -> String
        numberToColor number =
            if (number <= 1) then
                "#FDD"
            else if (number <= 3) then
                "#FFD"
            else if (number <= 5) then
                "#DFD"
            else if (number <= 7) then
                "#DFF"
            else
                "#DDF"

        countToStyle : ( Int, String ) -> List (Html.Attribute Msg)
        countToStyle ( freq, trigraph ) =
            [ Html.Attributes.style
                (( "backgroundColor", numberToColor freq )
                    :: (ColoredWordView.matchingStyle (trigraph == lexicalModel.currentTrigraph))
                )
            , onShiftedMouseEnter (LexicalMessage (SetCurrentTrigraph trigraph))
            ]

        renderChar : ( String, String ) -> Html Msg
        renderChar ( c, c3 ) =
            let
                count =
                    Dict.get c3 lenInfo |> Maybe.withDefault 0
            in
                span (countToStyle ( count, c3 )) [ text c ]
    in
        List.map renderChar charAndTriplet


frequencyStats : LexicalModel -> Html Msg
frequencyStats lexicalModel =
    Html.div []
        [ Html.table
            []
            [ tr []
                [ td [ cellStyle "800px" ]
                    (renderWords lexicalModel)
                , td [ cellStyle "800px" ]
                    [ FreqInfoView.renderFrequencies lexicalModel.workingNormalized (Dict.get 1 lexicalModel.frequencies) ]
                , td [ cellStyle "400px" ]
                    [ FreqInfoView.renderFrequencies lexicalModel.workingNormalized (Dict.get 2 lexicalModel.frequencies) ]
                ]
            ]
        , Html.table
            []
            [ tr []
                [ td [ cellStyle "600px" ]
                    (renderGraphs lexicalModel)
                , td [ cellStyle "1600px" ]
                    [ FreqInfoView.renderNgraphs lexicalModel.currentTrigraph (Dict.get 3 lexicalModel.ngraphs) ]
                ]
            ]
        ]


stylesheet : Css.Stylesheet Id Class msg
stylesheet =
    Css.stylesheet imports rules


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
