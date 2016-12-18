module BowlingScore exposing (testResults)

import List exposing (foldl)
import Testing exposing (TestResult)


type alias Throws =
    List Int


type alias Score =
    Int


score : Throws -> Score
score throws =
    List.foldl (+) 0 throws


testResults : List TestResult
testResults =
    testGameScore ++ testFrameIfication


testGameScore : List TestResult
testGameScore =
    (let
        testGame : { throws : List Int, expected : Int } -> TestResult
        testGame { throws, expected } =
            let
                computed =
                    score throws

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
            ]
    )


testFrameIfication : List TestResult
testFrameIfication =
    (let
        testGame : { throws : List Int, expected : List Frame } -> TestResult
        testGame { throws, expected } =
            let
                computed =
                    frameify throws

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
            , { throws = [ 1, 1 ], expected = [ Open 2 ] }
            , { throws = [ 1, 1, 1 ], expected = [ Open 2, Partial 1 ] }
            ]
    )


frameify : Throws -> List Frame
frameify throws =
    []


type Frame
    = Partial Int
    | Open Int
