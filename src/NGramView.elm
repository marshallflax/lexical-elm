module NGramView exposing (..)

import Dict exposing (..)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import NGram exposing (..)
import Types exposing (..)
import ColoredWordView exposing (..)


renderFrequencies : String -> FreqInfo -> Html Msg
renderFrequencies currentWord ( x, freqToWords ) =
    table
        [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ]
        ]
        (List.map
            (renderFrequency currentWord)
            (List.reverse (Dict.toList freqToWords))
        )


renderFrequency : String -> ( Int, (List String) ) -> Html Msg
renderFrequency currentWord ( size, words ) =
    tr []
        [ td [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            [ text (toString size) ]
        , renderWords currentWord words
        ]


renderWords : String -> List String -> Html Msg
renderWords currentWord words =
    let
        doWord : String -> Html Msg
        doWord word =
            span
                [ onClick (SetCurrentNormalized word)
                , style (matchingStyle (word == currentWord))
                ]
                [ text (" " ++ word ++ " ") ]
    in
        td
            [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            (List.map doWord words)
