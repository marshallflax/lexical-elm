module FreqInfoView exposing (renderFrequencies, renderNgraphs)

import ColoredWordView exposing (matchingStyle)
import Dict exposing (Dict)
import FreqInfo exposing (LenInfo, emptyLenInfo)
import Html exposing (Html, table, tr, td, text, span)
import Html.Attributes exposing (style)
import MiscView exposing (onShiftedMouseEnter)
import Set exposing (Set)
import Types exposing (..)


renderGeneric : (String -> Html Msg) -> Maybe LenInfo -> Html Msg
renderGeneric renderWord freqToWords =
    let
        solidBorder : Html.Attribute msg
        solidBorder =
            style [ ( "border", "solid" ), ( "border-width", "1px" ) ]

        renderFrequency : ( Int, List String ) -> Html Msg
        renderFrequency ( size, words ) =
            tr []
                [ td [ solidBorder ] [ text (toString size) ]
                , td [ solidBorder ] (List.map renderWord words)
                ]
    in
        table
            [ solidBorder ]
            (List.map renderFrequency <| List.reverse <| Dict.toList <| .freqToList <| Maybe.withDefault emptyLenInfo freqToWords)


renderFrequencies : Set String -> Maybe LenInfo -> Html Msg
renderFrequencies currentWordsNormalized freqToWords =
    let
        renderWord word =
            span
                [ onShiftedMouseEnter (LexicalMessage (SetCurrentNormalized word))
                , style (matchingStyle (Set.member word currentWordsNormalized))
                ]
                [ text (" <" ++ word ++ "> ") ]
    in
        renderGeneric renderWord freqToWords


renderNgraphs : String -> Maybe LenInfo -> Html Msg
renderNgraphs currentTrigraph freqToWords =
    let
        renderWord word =
            span
                [ onShiftedMouseEnter (LexicalMessage (SetCurrentTrigraph word))
                , style (matchingStyle (word == currentTrigraph))
                ]
                [ text (" <" ++ word ++ "> ") ]
    in
        renderGeneric renderWord freqToWords
