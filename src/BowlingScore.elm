module BowlingScore exposing (testResults)

import Array exposing (..)
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
            , { throws = [ 1, 1 ], expected = [ Open 1 1 ] }
            , { throws = [ 1, 1, 1 ], expected = [ Open 1 1, Partial 1 ] }
            ]
    )


type Frame
    = Partial Int
    | Open Int Int


frameify : Throws -> List Frame
frameify throws =
    let
        ( remainingThrows, computedFrame ) =
            frameHelper throws Array.empty
    in
        Array.toList computedFrame


frameHelper : Throws -> Array Frame -> ( Throws, Array Frame )
frameHelper throws currentFrames =
    let
        head =
            List.head throws

        next =
            List.head (List.drop 1 throws)
    in
        case head of
            Nothing ->
                ( throws, currentFrames )

            Just throw1 ->
                case next of
                    Nothing ->
                        frameHelper (List.drop 2 throws) (Array.push (Partial throw1) currentFrames)

                    Just throw2 ->
                        frameHelper (List.drop 2 throws) (Array.push (Open throw1 throw2) currentFrames)
