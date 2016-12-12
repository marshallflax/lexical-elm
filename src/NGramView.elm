module NGramView exposing (..)

import Dict exposing (..)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import NGram exposing (..)
import Types exposing (..)
import ColoredWordView exposing (..)


renderFrequencies : String -> FreqInfo -> Html Msg
renderFrequencies currentWord ( x, freqToWordSet ) =
    table
        [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ]
        ]
        (List.map
            (renderFrequency currentWord)
            (List.reverse (Dict.toList freqToWordSet))
        )


renderFrequency : String -> ( Int, WordToSet ) -> Html Msg
renderFrequency currentWord ( size, wordToSet ) =
    tr []
        [ td [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            [ text (toString size) ]
        , renderSet currentWord wordToSet
        ]


renderSet : String -> WordToSet -> Html Msg
renderSet currentWord wordToSet =
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
            (List.map doWord (Dict.keys wordToSet))
