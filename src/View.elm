module View exposing (viewSubscriptions, root)

import Array
import BowlingScoreView
import ColoredWord exposing (ColoredWord, matchingWordsForColor)
import ColoredWordView exposing (colorStyle, renderWord)
import Controller exposing (countWords, countWordsMatching, currentWordFromIndex, dumpState, partitionedList, rainbowList)
import Css
import DragView
import FreqInfoView exposing (renderFrequencies)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Keyboard
import Set exposing (Set)
import Types exposing (..)


viewSubscriptions : Model -> Sub Msg
viewSubscriptions _ =
    Keyboard.downs KeyMsg


enableButton : List String -> Html Msg
enableButton cs =
    button [ onClick (LexicalMessage (HideSomeColors cs)) ] [ text "hide" ]


disableButton : List String -> Html Msg
disableButton cs =
    button [ onClick (LexicalMessage (ResetSomeColors cs)) ] [ text "reset" ]


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


root : Model -> Html Msg
root model =
    div []
        [ Css.style [ Html.Attributes.scoped True ] stylesheet
        , DragView.viewDraggables model.draggables
        , span
            []
            [ text (toString (Set.toList ((currentWordFromIndex model.workingWord model).colors))) ]
        , p [] []
        , button [ onClick SaveModel ] [ text "Save" ]
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
                [ value (toString model.wordsPerLine)
                , onInput SetWordsPerLine
                ]
                [ text "WordsPerLine" ]
            , span []
                [ text
                    (toString (countWordsMatching model)
                        ++ "/"
                        ++ (toString (countWords model))
                    )
                ]
            ]
        , input
            [ value (String.join ", " (matchingWordsForColor model.workingColor model.words |> Array.toList))
            , style [ ( "width", "800px" ) ]
            , readonly True
            , colorStyle model.workingColor
            ]
            []
        , Html.table
            []
            [ tr []
                [ td [ style [ ( "width", "800px" ), ( "vertical-align", "top" ) ] ]
                    (let
                        doWord : ( Int, ColoredWord ) -> Html Msg
                        doWord =
                            renderWord model.hideColors model.workingColor model.workingNormalized

                        renderLine : List ( Int, ColoredWord ) -> Html Msg
                        renderLine listPart =
                            Html.div [ stylesheet.class MyClass ]
                                (List.map doWord listPart)
                     in
                        List.map renderLine (partitionedList model)
                    )
                , td [ style [ ( "width", "800px" ), ( "vertical-align", "top" ) ] ]
                    [ FreqInfoView.renderFrequencies model.workingNormalized model.frequencies.words ]
                , td [ style [ ( "width", "400px" ), ( "vertical-align", "top" ) ] ]
                    [ FreqInfoView.renderFrequencies model.workingNormalized model.frequencies.n2 ]
                ]
            ]
        , p [] [ text (toString model.lastKeyCode) ]
        , p [ style [ ( "fontSize", "20%" ) ] ]
            [ text "(c) marshall.flax@gmail.com; licensed "
            , Html.a [ href "https://www.gnu.org/licenses/gpl-3.0.en.html" ] [ text "GPL3.0 +" ]
            , text " "
            , Html.a [ href "https://github.com/marshallflax/lexical-elm" ] [ text "source" ]
            , text " "
            , Html.a [ href "https://raw.githubusercontent.com/marshallflax/lexical-elm/master/index.html" ] [ text "latest" ]
            ]
        , table []
            [ tr []
                [ td [] [ BowlingScoreView.showTestResultsOld model.bowlingResults ]
                , td [] [ BowlingScoreView.showTestResults model.tableState model.bowlingResults ]
                ]
            ]
        ]
