module BowlingScoreTest exposing (testResults)

import Array
import BowlingScore exposing (..)
import Testing exposing (TestResult)


testResults : List (Int, TestResult)
testResults =
    let
        results : List TestResult
        results = testGameScore ++ testFrameIfication
    in
        List.map (\result -> (0, result)) results


testGameScore : List TestResult
testGameScore =
    let
        testGame : { throws : List Int, expected : Int } -> TestResult
        testGame { throws, expected } =
            let
                computed =
                    score (Array.fromList throws)

                context =
                    (toString throws) ++ " => " ++ (toString expected)
            in
                if (computed == expected) then
                    Ok context
                else
                    Err (context ++ " but " ++ (toString computed))
    in
        List.map testGame
            [ { throws = [ 1 ], expected = 1 }
            , { throws = [ 1, 1 ], expected = 2 }
            , { throws = [ 1, 1, 1 ], expected = 2 }
            , { throws = [ 10, 1 ], expected = 12 }
            , { throws = [ 10, 1, 3 ], expected = 18 }
            , { throws = [ 3, 7, 1, 3 ], expected = 15 }
            , { throws = List.repeat 12 10, expected = 300 }
            ]


testFrameIfication : List TestResult
testFrameIfication =
    let
        testGame : { throws : List Int, expected : List Frame } -> TestResult
        testGame { throws, expected } =
            let
                computed =
                    frameify (Array.fromList throws) |> Array.toList

                context =
                    (toString throws) ++ " => " ++ (toString expected)
            in
                if (computed == expected) then
                    Ok context
                else
                    Err (context ++ " but " ++ (toString computed))
    in
        List.map testGame
            [ { throws = [ 1 ], expected = [ Partial 1 ] }
            , { throws = [ 1, 1 ], expected = [ Open 1 1 ] }
            , { throws = [ 1, 1, 1 ], expected = [ Open 1 1, Partial 1 ] }
            , { throws = [ 10, 1, 2 ], expected = [ Strike, Open 1 2 ] }
            , { throws = [ 3, 7, 1, 2 ], expected = [ Spare 3, Open 1 2 ] }
            ]
