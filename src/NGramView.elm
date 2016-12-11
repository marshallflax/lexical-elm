module NGramView exposing (..)

import Dict exposing (..)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
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
            span [] [ text (" " ++ word ++ " ") ]
    in
        td
        []
            (List.map doWord (Dict.keys wordToSet))
