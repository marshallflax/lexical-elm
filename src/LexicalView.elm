module LexicalView exposing (viewLexicalModel, stylesheet)

import Array
import ColoredWord exposing (ColoredWord, matchingWordsForColor)
import ColoredWordView exposing (colorStyle, renderWord)
import Css
import FreqInfoView exposing (renderFrequencies)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy exposing (lazy)
import LexicalController exposing (countWords, countWordsMatching, currentWordFromIndex, dumpState, partitionedList, rainbowList)
import Misc
import Set exposing (Set)
import Types exposing (..)


viewLexicalModel : LexicalModel -> Html Msg
viewLexicalModel lexicalModel =
    div []
        [ showColorsOfCurrentWord lexicalModel
        , p [] [Misc.zipLists rainbowList |> toString |> text]
        , showSaveButton
        , showTextInput lexicalModel
        , lazy colorButtons lexicalModel.hideColors
        , p [] []
        , resetButtons
        , wordsPerLine lexicalModel
        , wordStats lexicalModel
        , wordsForColor lexicalModel
        , frequencyStats lexicalModel
        ]


showColorsOfCurrentWord : LexicalModel -> Html Msg
showColorsOfCurrentWord lexicalModel =
    span []
        [ currentWordFromIndex lexicalModel.workingWord lexicalModel
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
        [ value (dumpState lexicalModel)
        , onInput (LexicalMessage << SetText)
        , style [ ( "width", "800px" ) ]
        ]
        []


colorButtons : Set String -> Html Msg
colorButtons hideColors =
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
                td []
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


frequencyStats : LexicalModel -> Html Msg
frequencyStats lexicalModel =
    Html.table
        []
        [ tr []
            [ td [ style [ ( "width", "800px" ), ( "vertical-align", "top" ) ] ]
                (let
                    doWord : ( Int, ColoredWord ) -> Html Msg
                    doWord =
                        renderWord lexicalModel.hideColors lexicalModel.workingColor lexicalModel.workingNormalized

                    renderLine : List ( Int, ColoredWord ) -> Html Msg
                    renderLine listPart =
                        Html.div [ stylesheet.class MyClass ]
                            (List.map doWord listPart)
                 in
                    List.map renderLine (partitionedList lexicalModel)
                )
            , td [ style [ ( "width", "800px" ), ( "vertical-align", "top" ) ] ]
                [ FreqInfoView.renderFrequencies lexicalModel.workingNormalized lexicalModel.frequencies.words ]
            , td [ style [ ( "width", "400px" ), ( "vertical-align", "top" ) ] ]
                [ FreqInfoView.renderFrequencies lexicalModel.workingNormalized lexicalModel.frequencies.n2 ]
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
