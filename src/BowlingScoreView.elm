module BowlingScoreView exposing (initialTableState, showTestResults, showTestResultsOld)

import Html exposing (Html, table, tr, td, text)
import Html.Attributes exposing (style)
import Table
import Testing exposing (TestResult)
import Types exposing (Msg(SetTableState))


initialTableState : Table.State
initialTableState =
    Table.initialSort "Text"


showTestResults : Table.State -> List TestResult -> Html Msg
showTestResults tableState testList =
    let
        tableConfig : Table.Config TestResult Msg
        tableConfig =
            Table.config
                { toId = toString
                , toMsg = SetTableState
                , columns =
                    [ Table.stringColumn "Text" toString
                    ]
                }
    in
        Table.view tableConfig tableState testList


showTestResultsOld : List TestResult -> Html Msg
showTestResultsOld testList =
    let
        doTest : TestResult -> Html Msg
        doTest scoreTest =
            case scoreTest of
                Result.Ok t ->
                    tr [ style [ ( "backgroundColor", "lightgreen" ) ] ] [ td [] [ (text t) ] ]

                Result.Err t ->
                    tr [ style [ ( "backgroundColor", "red" ) ] ] [ td [] [ (text t) ] ]
    in
        table [] (List.map doTest testList)
