module NGramView exposing (renderFrequencies)

import ColoredWordView exposing (matchingStyle)
import Dict exposing (..)
import Html exposing (Html, table, tr, td, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Types exposing (..)


renderFrequencies : String -> Dict Int (List String) -> Html Msg
renderFrequencies currentWord freqToWords =
    table
        [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ]
        ]
        (List.map
            (renderFrequency currentWord)
            (List.reverse (Dict.toList freqToWords))
        )


renderFrequency : String -> ( Int, List String ) -> Html Msg
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
