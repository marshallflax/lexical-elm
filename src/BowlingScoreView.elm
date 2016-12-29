module BowlingScoreView exposing (initialTableState, showTestResults, showTestResultsOld)

import Html exposing (Html, table, tr, td, text)
import Html.Attributes exposing (style)
import Table
import Testing exposing (TestResult)
import Types exposing (Msg(SetTableState))


initialTableState : Table.State
initialTableState =
    Table.initialSort "Which"


showTestResults : Table.State -> List ( Int, TestResult ) -> Html Msg
showTestResults =
    Table.view
        (Table.config
            { toId = Tuple.first >> toString
            , toMsg = SetTableState
            , columns =
                [ Table.intColumn "Which" Tuple.first
                , Table.stringColumn "Text" (Tuple.second >> toString)
                ]
            }
        )


showTestResultsOld : List ( Int, TestResult ) -> Html Msg
showTestResultsOld testList =
    let
        doTest : ( Int, TestResult ) -> Html Msg
        doTest ( i, scoreTest ) =
            case scoreTest of
                Result.Ok t ->
                    tr [ style [ ( "backgroundColor", "lightgreen" ) ] ]
                        [ td [] [ (text (toString i)) ]
                        , td [] [ (text t) ]
                        ]

                Result.Err t ->
                    tr [ style [ ( "backgroundColor", "red" ) ] ]
                        [ td [] [ (text (toString i)) ]
                        , td [] [ (text t) ]
                        ]
    in
        table [] (List.map doTest testList)
