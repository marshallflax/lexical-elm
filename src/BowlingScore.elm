module BowlingScore exposing (..)

import List exposing (foldl)


type alias Throws =
    List Int


type alias Score =
    Int


type alias ScoreTest =
    { throws : Throws, expectedScore : Score }


tests : List ScoreTest
tests =
    [ { throws = [ 1 ], expectedScore = 1 }
    , { throws = [ 1, 1 ], expectedScore = 2 }
    , { throws = [ 1, 1, 1 ], expectedScore = 2 }
    ]

type alias TestResult =
    Result String String

runTest : ScoreTest -> TestResult
runTest st =
    let
        computed =
            score st.throws

        description =
            (toString st.throws) ++ " expected " ++ (toString st.expectedScore)
    in
        if (computed == st.expectedScore) then
            Ok description
        else
            Err (description ++ " but got " ++ (toString computed))


score : Throws -> Score
score throws =
    List.foldl (+) 0 throws
