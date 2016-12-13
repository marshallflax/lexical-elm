module View exposing (root)

import ColoredWord exposing (ColoredWord, matchingWordsForColor)
import ColoredWordView exposing (..)
import Css
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Set exposing (Set)
import State exposing (..)
import Types exposing (..)
import NGramView exposing (..)


enableButton : List String -> Html Msg
enableButton cs =
    button [ onClick (HideSomeColors cs) ] [ text "hide" ]


disableButton : List String -> Html Msg
disableButton cs =
    button [ onClick (ResetSomeColors cs) ] [ text "reset" ]


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
        , span
            []
            [ text (toString (Set.toList ((currentWordFromIndex model.workingWord model).colors))) ]
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
            [ value (String.join ", " (matchingWordsForColor model.workingColor model.words))
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
                    [ NGramView.renderFrequencies model.workingNormalized model.frequencies.words ]
                , td [ style [ ( "width", "800px" ), ( "vertical-align", "top" ) ] ]
                    [ NGramView.renderFrequencies model.workingNormalized model.frequencies.n2 ]
                ]
            ]
        , p [ style [ ( "fontSize", "10%" ) ] ]
            [ text "(c) marshall.flax@gmail.com; licensed "
            , Html.a [ href "https://www.gnu.org/licenses/gpl-3.0.en.html" ] [ text "GPL3.0 +" ]
            , text " "
            , Html.a [ href "https://github.com:marshallflax/lexical-elm" ] [ text "source" ]
            , text " "
            , Html.a [ href "https://raw.githubusercontent.com/marshallflax/lexical-elm/master/index.html" ] [ text "latest" ]
            ]
        ]
