module BowlingScoreView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Testing exposing (..)
import Types exposing (..)

showTestResults : List TestResult -> Html Msg
showTestResults testList =
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
