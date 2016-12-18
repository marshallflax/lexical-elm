module BowlingScore exposing (..)

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
    let
        testGame { throws, expectedScore } =
            let
                computed =
                    score throws

                description =
                    (toString throws) ++ " expected " ++ (toString expectedScore)
            in
                if (computed == expectedScore) then
                    Ok description
                else
                    Err (description ++ " but got " ++ (toString computed))
    in
        List.map testGame
            [ { throws = [ 1 ], expectedScore = 1 }
            , { throws = [ 1, 1 ], expectedScore = 2 }
            , { throws = [ 1, 1, 1 ], expectedScore = 2 }
            ]



--     ++    List.map testParsing        [ { throws = [1], expected =          ]
