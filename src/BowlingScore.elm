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
            , { throws = [ 10, 1, 2 ], expected = [ Strike, Open 1 2 ] }
            , { throws = [ 3, 7, 1, 2 ], expected = [ Spare 3, Open 1 2 ] }
            ]
    )


type Frame
    = Partial Int
    | Open Int Int
    | Spare Int
    | Strike


frameify : Throws -> List Frame
frameify throws =
    let
        ( remainingThrows, computedFrame ) =
            frameHelper throws Array.empty
    in
        Array.toList computedFrame


frameHelper : Throws -> Array Frame -> ( Throws, Array Frame )
frameHelper throws currentFrames =
    case List.head throws of
        Nothing ->
            ( throws, currentFrames )

        Just throw1 ->
            if (throw1 == 10) then
                frameHelper (List.drop 1 throws) (Array.push (Strike) currentFrames)
            else
                case List.head (List.drop 1 throws) of
                    Nothing ->
                        frameHelper (List.drop 2 throws) (Array.push (Partial throw1) currentFrames)

                    Just throw2 ->
                        if (throw1 + throw2 == 10) then
                            frameHelper (List.drop 2 throws) (Array.push (Spare throw1) currentFrames)
                        else
                            frameHelper (List.drop 2 throws) (Array.push (Open throw1 throw2) currentFrames)
