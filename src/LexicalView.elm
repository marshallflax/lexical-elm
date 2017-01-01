module LexicalView exposing (viewLexicalModel, stylesheet)

import Array
import ColoredWord exposing (ColoredWord, matchingWordsForColor)
import ColoredWordView exposing (colorStyle, renderWord)
import Css
import FreqInfoView exposing (renderFrequencies)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import LexicalController exposing (countWords, countWordsMatching, currentWordFromIndex, dumpState, partitionedList, rainbowList)
import Set exposing (Set)
import Types exposing (..)


viewLexicalModel : LexicalModel -> Html Msg
viewLexicalModel lexicalModel =
    div []
        [ span [] [ text (toString (Set.toList ((currentWordFromIndex lexicalModel.workingWord lexicalModel).colors))) ]
        , p [] []
        , button [ onClick SaveModel ] [ text "Save" ]
        , input
            [ value (dumpState lexicalModel)
            , onInput (\text -> LexicalMessage (SetText text))
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
                            , onClick (LexicalMessage (ToggleColorEnabled l))
                            , checked (Set.member l lexicalModel.hideColors)
                            ]
                            []
                        , button [ Html.Attributes.attribute "id" ("colorButton" ++ l), colorStyle l, onClick (LexicalMessage (SetCurrentColor l)) ] [ text l ]
                        ]

                doRow : List String -> Html Msg
                doRow ls =
                    table [] [ tr [] (enableButton ls :: disableButton ls :: (List.map doCell ls)) ]
             in
                List.map doRow rainbowList
            )
        , Html.p
            []
            [ button [ onClick (LexicalMessage EnableAllColors) ] [ text "ResetHiding" ]
            , input
                [ value (toString lexicalModel.wordsPerLine)
                , onInput (\number -> LexicalMessage (SetWordsPerLine number))
                ]
                [ text "WordsPerLine" ]
            , span []
                [ text
                    (toString (countWordsMatching lexicalModel)
                        ++ "/"
                        ++ (toString (countWords lexicalModel))
                    )
                ]
            ]
        , input
            [ value (String.join ", " (matchingWordsForColor lexicalModel.workingColor lexicalModel.words |> Array.toList))
            , style [ ( "width", "800px" ) ]
            , readonly True
            , colorStyle lexicalModel.workingColor
            ]
            []
        , Html.table
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
        ]


enableButton : List String -> Html Msg
enableButton cs =
    button [ onClick (LexicalMessage (HideSomeColors cs)) ] [ text "hide" ]


disableButton : List String -> Html Msg
disableButton cs =
    button [ onClick (LexicalMessage (ResetSomeColors cs)) ] [ text "reset" ]


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
