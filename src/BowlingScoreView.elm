module BowlingScoreView exposing (initialTableState, showTestResults, showTestResultsOld)

import Html exposing (Html, table, tr, td, text)
import Html.Attributes exposing (style)
import Table exposing (defaultCustomizations)
import Testing exposing (TestResult)
import Types exposing (Msg(SetTableState))


initialTableState : Table.State
initialTableState =
    Table.initialSort "Which"


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
toRowAttrs ( row, result ) =
    [ style
        [ ( "background"
          , case result of
                Result.Ok _ ->
                    "lightgreen"

                Result.Err _ ->
                    "red"
          )
        ]
    ]


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
