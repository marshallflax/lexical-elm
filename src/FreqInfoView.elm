module FreqInfoView exposing (renderFrequencies, renderNgraphs)

import ColoredWordView exposing (matchingStyle)
import Dict exposing (Dict)
import FreqInfo exposing (LenInfo, emptyLenInfo)
import Html exposing (Html, table, tr, td, text, span)
import Html.Attributes exposing (style)
import MiscView exposing (onShiftedMouseEnter)
import Set exposing (Set)
import Types exposing (..)


renderGeneric : (String -> List (Html.Attribute Msg)) -> Set String -> Maybe LenInfo -> Html Msg
renderGeneric computeStyle currentWordsNormalized freqToWords =
    let
        doWord : String -> Html Msg
        doWord word =
            span
                (computeStyle word)
                [ text (" <" ++ word ++ "> ") ]

        renderWords : List String -> Html Msg
        renderWords words =
            td
                [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
                (List.map doWord words)

        renderFrequency : ( Int, List String ) -> Html Msg
        renderFrequency ( size, words ) =
            tr []
                [ td [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
                    [ text (toString size) ]
                , renderWords words
                ]
    in
        table
            [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            (List.map renderFrequency <| List.reverse <| Dict.toList <| .freqToList <| Maybe.withDefault emptyLenInfo freqToWords)


renderFrequencies : Set String -> Maybe LenInfo -> Html Msg
renderFrequencies currentWordsNormalized freqToWords =
    let
        computeStyle : String -> List (Html.Attribute Msg)
        computeStyle word =
            [ onShiftedMouseEnter (LexicalMessage (SetCurrentNormalized word))
            , style (matchingStyle (Set.member word currentWordsNormalized))
            ]
    in
        renderGeneric computeStyle currentWordsNormalized freqToWords


renderNgraphs : Maybe LenInfo -> Html Msg
renderNgraphs freqToWords =
    let
        computeStyle : String -> List (Html.Attribute Msg)
        computeStyle word =
            []

        currentWordsNormalized : Set String
        currentWordsNormalized =
            Set.empty
    in
        renderGeneric computeStyle currentWordsNormalized freqToWords
