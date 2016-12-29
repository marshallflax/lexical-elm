module BowlingScoreView exposing (initialTableState, showTestResults, showTestResultsOld)

import Html exposing (Html, table, tr, td, text)
import Html.Attributes exposing (style)
import Table
import Testing exposing (TestResult)
import Types exposing (Msg(SetTableState))


initialTableState : Table.State
initialTableState =
    Table.initialSort "Text"


showTestResults : Table.State -> List ( Int, TestResult ) -> Html Msg
showTestResults tableState numberedTestList =
    let
        tableConfig : Table.Config ( Int, TestResult ) Msg
        tableConfig =
            Table.config
                { toId = toString
                , toMsg = SetTableState
                , columns =
                    [ Table.stringColumn "Text" (\( row, data ) -> toString data)
                    ]
                }
    in
        Table.view tableConfig tableState numberedTestList


showTestResultsOld : List ( Int, TestResult ) -> Html Msg
showTestResultsOld testList =
    let
        doTest : ( Int, TestResult ) -> Html Msg
        doTest ( i, scoreTest ) =
            case scoreTest of
                Result.Ok t ->
                    tr [ style [ ( "backgroundColor", "lightgreen" ) ] ] [ td [] [ (text t) ] ]

                Result.Err t ->
                    tr [ style [ ( "backgroundColor", "red" ) ] ] [ td [] [ (text t) ] ]
    in
        table [] (List.map doTest testList)
