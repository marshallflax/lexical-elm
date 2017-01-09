module BowlingScore exposing (score, scoreList, frameify, Frame(..))

import Array exposing (Array)
import List exposing (foldl)
import Transducer exposing (map, (>>>))
import StatefulTransducer


type alias Throws =
    Array Int


type alias Score =
    Int


type Frame
    = Empty
    | Partial Int
    | Open Int Int
    | Spare Int
    | Strike


type Mode
    = PostOpen
    | PostSpare
    | PostStrike
    | PostDoubleStrike


naivePoints : Frame -> Score
naivePoints frame =
    case frame of
        Strike ->
            10

        Spare _ ->
            10

        Open t1 t2 ->
            t1 + t2

        Partial t1 ->
            t1

        Empty ->
            0

firstThrowPoints : Frame -> Score
firstThrowPoints frame =
    case frame of
        Strike ->
            10

        Spare t1 ->
            t1

        Open t1 _ ->
            t1

        Partial t1 ->
            t1

        Empty ->
            0


computeNormalNextMode : Frame -> Mode
computeNormalNextMode frame =
    case frame of
        Strike ->
            PostStrike

        Spare _ ->
            PostSpare

        Open _ _ ->
            PostOpen

        Partial _ ->
            PostOpen

        Empty ->
            PostOpen

computePostStrikeMode : ( Frame, Int ) -> Mode
computePostStrikeMode ( frame, whichFrame ) =
    case frame of
        Strike ->
            if (whichFrame <= 10) then
                PostDoubleStrike
            else
                PostSpare

        Spare _ ->
            PostSpare

        Open _ _ ->
            PostOpen

        Partial _ ->
            PostOpen

        Empty ->
            PostOpen

type alias WorkingState =
    ( Mode, Score, Int )


scoreFold : Frame -> WorkingState -> WorkingState
scoreFold frame ( currentMode, currentScore, whichFrame ) =
    let
        basePoints =
            if (whichFrame <= 10) then
                currentScore + (naivePoints frame)
            else
                currentScore

        nextFrame =
            whichFrame + 1
    in
        case currentMode of
            PostOpen ->
                ( computeNormalNextMode frame, basePoints, nextFrame )

            PostSpare ->
                ( computeNormalNextMode frame, basePoints + firstThrowPoints frame, nextFrame )

            PostStrike ->
                ( computePostStrikeMode ( frame, whichFrame ), basePoints + naivePoints frame, nextFrame )

            PostDoubleStrike ->
                ( computePostStrikeMode ( frame, whichFrame ), basePoints + 2 * (naivePoints frame), nextFrame )

scoreList : List Int -> Score
scoreList =
    Array.fromList >> score

score : Throws -> Score
score throws =
    let
        ( finalMode, finalScore, finalFrame ) =
            Array.foldl scoreFold ( PostOpen, 0, 1 ) (frameify throws)
    in
        finalScore


listToFrame : List Int -> Frame
listToFrame throws =
    case List.head throws of
        Nothing ->
            Empty

        Just throw1 ->
            if (throw1 == 10) then
                Strike
            else
                case List.head (List.drop 1 throws) of
                    Nothing ->
                        Partial throw1

                    Just throw2 ->
                        if (throw1 + throw2 == 10) then
                            Spare throw1
                        else
                            Open throw1 throw2


sumArray : Array number -> number
sumArray arr =
    Array.foldl (+) 0 arr


isCompleteFrame : Array Int -> Bool
isCompleteFrame throws =
    (Array.length throws >= 2) || (sumArray throws >= 10)


frameify : Array Int -> Array Frame
frameify =
    Transducer.transduceArray
        (StatefulTransducer.statefulPartitionBy isCompleteFrame
            >>> Transducer.map Array.toList
            >>> Transducer.map listToFrame
        )
