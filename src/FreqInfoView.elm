module FreqInfoView exposing (renderFrequencies)

import ColoredWordView exposing (matchingStyle)
import Dict exposing (..)
import Html exposing (Html, table, tr, td, text, span)
import Html.Attributes exposing (style)
import MiscView exposing (..)
import Set exposing (..)
import Types exposing (..)


renderFrequencies : Set String -> Maybe (Dict Int (List String)) -> Html Msg
renderFrequencies currentWordsNormalized freqToWords =
    table
        [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ]
        ]
        (List.map
            (renderFrequency currentWordsNormalized)
            (List.reverse (Dict.toList (Maybe.withDefault Dict.empty freqToWords)))
        )


renderFrequency : Set String -> ( Int, List String ) -> Html Msg
renderFrequency currentWordsNormalized ( size, words ) =
    tr []
        [ td [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            [ text (toString size) ]
        , renderWords currentWordsNormalized words
        ]


renderWords : Set String -> List String -> Html Msg
renderWords currentWordsNormalized words =
    let
        doWord : String -> Html Msg
        doWord word =
            span
                [ onShiftedMouseEnter (LexicalMessage (SetCurrentNormalized word))
                , style (matchingStyle (Set.member word currentWordsNormalized))
                ]
                [ text (" <" ++ word ++ "> ") ]
    in
        td
            [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            (List.map doWord words)
