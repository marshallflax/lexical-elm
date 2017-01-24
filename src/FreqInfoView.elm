module FreqInfoView exposing (renderFrequencies, renderNgraphs)

import ColoredWordView exposing (matchingStyle)
import Dict exposing (..)
import Html exposing (Html, table, tr, td, text, span)
import Html.Attributes exposing (style)
import MiscView exposing (..)
import Set exposing (..)
import Types exposing (..)


renderFrequencies : Set String -> Maybe (Dict Int (List String)) -> Html Msg
renderFrequencies currentWordsNormalized freqToWords =
    let
        doWord : String -> Html Msg
        doWord word =
            span
                [ onShiftedMouseEnter (LexicalMessage (SetCurrentNormalized word))
                , style (matchingStyle (Set.member word currentWordsNormalized))
                ]
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
            (List.map renderFrequency (List.reverse (Dict.toList (Maybe.withDefault Dict.empty freqToWords))))


renderNgraphs : Maybe (Dict Int (List String)) -> Html Msg
renderNgraphs freqToWords =
    let
        doInstance : String -> Html Msg
        doInstance instance =
            span
                []
                [ text ("<" ++ instance ++ "> ") ]

        renderInstances : List String -> Html Msg
        renderInstances instances =
            td
                [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
                (List.map doInstance instances)

        renderFrequency : ( Int, List String ) -> Html Msg
        renderFrequency ( frequency, instances ) =
            tr []
                [ td [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
                    [ text (toString frequency) ]
                , renderInstances instances
                ]
    in
        table
            [ style [ ( "border", "solid" ), ( "border-width", "1px" ) ] ]
            (List.map renderFrequency (List.reverse (Dict.toList (Maybe.withDefault Dict.empty freqToWords))))
