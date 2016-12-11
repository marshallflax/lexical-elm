module NGramView exposing (..)

import Dict exposing (..)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import NGram exposing (..)
import Types exposing (..)


renderFrequencies : FreqInfo -> Html Msg
renderFrequencies ( _, freqToWordSet ) =
    table
        [ style [ ( "border", "solid" ) ]
        ]
        (List.map
            renderFrequency
            (List.reverse (Dict.toList freqToWordSet))
        )


renderFrequency : ( Int, WordToSet ) -> Html Msg
renderFrequency ( size, wordToSet ) =
    tr []
        [ td []
            [ text (toString size) ]
        , renderSet wordToSet
        ]


renderSet : WordToSet -> Html Msg
renderSet wordToSet =
    let
        doWord : String -> Html Msg
        doWord word =
            span
                [ onClick (SetCurrentNormalized word) ]
                [ text (" " ++ word ++ " ") ]
    in
        td
            [ style [ ( "border", "solid" ) ] ]
            (List.map doWord (Dict.keys wordToSet))
