module BowlingScoreView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Table
import Testing exposing (..)
import Types exposing (..)


showTestResults : Table.State -> List TestResult -> Html Msg
showTestResults tableState testList =
    Table.view tableConfig tableState testList


initialTableState : Table.State
initialTableState =
    Table.initialSort "Text"


tableConfig : Table.Config TestResult Msg
tableConfig =
    Table.config
        { toId = toString
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Text" toString
            ]
        }


showTestResultsOld : List TestResult -> Html Msg
showTestResultsOld testList =
    let
        doTest : TestResult -> Html Msg
        doTest scoreTest =
            case scoreTest of
                Result.Ok t ->
                    tr [ Html.Attributes.style [ ( "backgroundColor", "lightgreen" ) ] ] [ td [] [ (text t) ] ]

                Result.Err t ->
                    tr [ Html.Attributes.style [ ( "backgroundColor", "red" ) ] ] [ td [] [ (text t) ] ]
    in
        table [] (List.map doTest testList)
