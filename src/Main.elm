module Main exposing (..)

import Array exposing (Array)
import Css
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import List.Split
import Set exposing (Set)
import Types exposing (..)
import State exposing (..)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- main : Program Never

main: Program Never Model Msg
main =
    Html.program
        { init = (model, Cmd.none), view = myView, update = myUpdate, subscriptions = subscriptions }




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
        , p [ style [ ( "fontSize", "10%" ) ] ]
            [ text "(c) marshall.flax@gmail.com; licensed "
            , Html.a [ href "https://www.gnu.org/licenses/gpl-3.0.en.html" ] [ text "GPL3.0 +" ]
            ]
        ]
