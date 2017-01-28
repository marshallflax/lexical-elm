module LexicalView exposing (viewLexicalModel)

import Array
import ColoredWord exposing (ColoredWord, matchingWordsForColor)
import ColoredWordView exposing (colorStyle, renderWord)
import Dict exposing (Dict)
import DragView exposing (viewDraggable)
import FreqInfo
import FreqInfoView
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes
import Html.Events exposing (onClick, onInput, onMouseEnter)
import LexicalController exposing (countWords, countWordsMatching, currentWordFromIndex, dumpState, partitionedList, rainbowList)
import Misc
import MiscView exposing (onShiftedMouseEnter)
import Set exposing (Set)
import Styles
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
    span []
        [ currentWordFromIndex lexicalModel.workingWord lexicalModel.words
            |> .colors
            |> Set.toList
            |> toString
            |> text
        ]


showSaveButton : Html Msg
showSaveButton =
    button [ onClick SaveModel ] [ text "Save" ]


showTextInput : LexicalModel -> Html Msg
showTextInput lexicalModel =
    input
        [ Html.Attributes.value (dumpState lexicalModel)
        , onInput (LexicalMessage << SetText)
        , Styles.useClass Styles.Cell800px
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
                            [ Html.Attributes.type_ "checkbox"
                            , onClick (LexicalMessage (ToggleColorEnabled l))
                            , Html.Attributes.checked (Set.member l hideColors)
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
        [ Html.Attributes.value (toString lexicalModel.wordsPerLine)
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
        [ Html.Attributes.value
            (matchingWordsForColor lexicalModel.workingColor lexicalModel.words
                |> Array.toList
                |> String.join ", "
            )
        , Styles.useClass Styles.Cell800px
        , Html.Attributes.readonly True
        , colorStyle lexicalModel.workingColor
        ]
        []


renderWords : LexicalModel -> List (Html Msg)
renderWords lexicalModel =
    let
        doWord : ( Int, ColoredWord ) -> Html Msg
        doWord =
            renderWord lexicalModel.hideColors lexicalModel.workingColor lexicalModel.workingNormalized

        renderLine : List ( Int, ColoredWord ) -> Html Msg
        renderLine listPart =
            Html.div [ Styles.useClass Styles.NumberLineClass ]
                (List.map doWord listPart)
    in
        List.map renderLine (partitionedList lexicalModel)


renderGraphs : LexicalModel -> List (Html Msg)
renderGraphs lexicalModel =
    let
        chars : List String
        chars =
            String.toList lexicalModel.graphs |> List.map String.fromChar

        triplets : List String
        triplets =
            Misc.zipShifts " " (List.range 0 2) chars
                |> List.map String.concat

        tripletsTriples : List (List String)
        tripletsTriples =
            Misc.zipShifts " " (List.range 0 2) triplets

        charAndTriplets : List ( String, List String )
        charAndTriplets =
            List.map2 (,) chars tripletsTriples

        lenInfo : Dict String Int
        lenInfo =
            Dict.get 3 lexicalModel.ngraphs
                |> Maybe.withDefault FreqInfo.emptyLenInfo
                |> .tokenToCount

        numberToColor : Int -> String
        numberToColor number =
            if (number <= 1) then
                "#FCC"
            else if (number <= 3) then
                "#FFC"
            else if (number <= 5) then
                "#CFC"
            else if (number <= 7) then
                "#CFF"
            else
                "#CCF"

        countToStyle : ( List Int, String ) -> List (Html.Attribute Msg)
        countToStyle ( freqs, trigraph ) =
            [ Html.Attributes.style [ ( "background", "linear-gradient(135deg," ++ (String.join "," (List.map numberToColor freqs)) ++ ")" ) ]
            , ColoredWordView.matchingStyle (trigraph == lexicalModel.currentTrigraph)
            , Styles.useClass Styles.GradientClass
            , onShiftedMouseEnter (LexicalMessage (SetCurrentTrigraph trigraph))
            ]

        renderChar : ( String, List String ) -> Html Msg
        renderChar ( letter, triplets ) =
            let
                thisTrigraph : String
                thisTrigraph =
                    List.head triplets |> Maybe.withDefault ""

                counts : List Int
                counts =
                    List.map (\x -> Dict.get x lenInfo |> Maybe.withDefault 0) triplets
            in
                span (countToStyle ( counts, thisTrigraph )) [ text letter ]
    in
        List.map renderChar charAndTriplets


frequencyStats : LexicalModel -> Html Msg
frequencyStats lexicalModel =
    Html.div []
        [ Html.table
            []
            [ tr []
                [ td [ Styles.useClass Styles.Cell800px ]
                    (renderWords lexicalModel)
                , td [ Styles.useClass Styles.Cell800px ]
                    [ FreqInfoView.renderFrequencies lexicalModel.workingNormalized (Dict.get 1 lexicalModel.frequencies) ]
                , td [ Styles.useClass Styles.Cell800px ]
                    [ FreqInfoView.renderFrequencies lexicalModel.workingNormalized (Dict.get 2 lexicalModel.frequencies) ]
                ]
            ]
        , Html.table
            []
            [ tr []
                [ td [ Styles.useClass Styles.Cell800px ]
                    (renderGraphs lexicalModel)
                , td [ Styles.useClass Styles.Cell800px ]
                    [ FreqInfoView.renderNgraphs lexicalModel.currentTrigraph (Dict.get 3 lexicalModel.ngraphs) ]
                ]
            ]
        ]
