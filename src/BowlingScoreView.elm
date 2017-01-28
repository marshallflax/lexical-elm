module BowlingScoreView exposing (initialTableState, showTestResults, showTestResultsOld)

import Html exposing (Html, table, tr, td, text)
import Styles
import Table exposing (defaultCustomizations)
import Testing exposing (TestResult)
import Types exposing (Msg(SetTableState))


initialTableState : Table.State
initialTableState =
    Table.initialSort "Text"


showTestResults : Table.State -> List ( Int, TestResult ) -> Html Msg
showTestResults =
    Table.view
        (Table.customConfig
            { toId = Tuple.first >> toString
            , toMsg = SetTableState
            , columns =
                [ Table.intColumn "Which" Tuple.first
                , Table.stringColumn "Text" (Tuple.second >> toString)
                ]
            , customizations =
                { defaultCustomizations | rowAttrs = toRowAttrs }
            }
        )


toRowAttrs : ( Int, TestResult ) -> List (Html.Attribute Msg)
toRowAttrs ( _, result ) =
    case result of
        Result.Ok _ ->
            [ Styles.useClass Styles.TestGood ]

        Result.Err _ ->
            [ Styles.useClass Styles.TestBad ]


showTestResultsOld : List ( Int, TestResult ) -> Html Msg
showTestResultsOld testList =
    let
        doTest : ( Int, TestResult ) -> Html Msg
        doTest ( i, scoreTest ) =
            case scoreTest of
                Result.Ok t ->
                    tr [ Styles.useClass Styles.TestGood ]
                        [ td [] [ (text (toString i)) ]
                        , td [] [ (text t) ]
                        ]

                Result.Err t ->
                    tr [ Styles.useClass Styles.TestBad ]
                        [ td [] [ (text (toString i)) ]
                        , td [] [ (text t) ]
                        ]
    in
        table [] (List.map doTest testList)
