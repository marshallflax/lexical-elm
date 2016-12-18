module BowlingScore exposing (testResults)

import Array exposing (Array, empty, push)
import Debug
import List exposing (foldl)
import Testing exposing (TestResult)
import Transducer exposing (..)


type alias Throws =
    List Int


type alias Score =
    Int


type Frame
    = Partial Int
    | Open Int Int
    | Spare Int
    | Strike


type Mode
    = PostOpen
    | PostSpare
    | PostStrike
    | PostAndPostPostStrike


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
            , { throws = [ 10, 1 ], expected = 12 }
            , { throws = [ 10, 1, 3 ], expected = 18 }
            , { throws = [ 3, 7, 1, 3 ], expected = 15 }
            , { throws = List.repeat 12 10, expected = 300 }
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


{-| Recursively removes one or two throws from the first list and appends one frame to the second array -- until the first list is empty
-}
frameHelper : ( Throws, Array Frame ) -> ( Throws, Array Frame )
frameHelper ( throws, currentFrames ) =
    case List.head throws of
        Nothing ->
            ( throws, currentFrames )

        Just throw1 ->
            let
                recurse ( num, frame ) =
                    frameHelper ( (List.drop num throws), (Array.push frame currentFrames) )
            in
                if (throw1 == 10) then
                    recurse ( 1, Strike )
                else
                    case List.head (List.drop 1 throws) of
                        Nothing ->
                            recurse ( 1, Partial throw1 )

                        Just throw2 ->
                            if (throw1 + throw2 == 10) then
                                recurse ( 2, Spare throw1 )
                            else
                                recurse ( 2, Open throw1 throw2 )


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


computePostStrikeMode : ( Frame, Int ) -> Mode
computePostStrikeMode ( frame, whichFrame ) =
    case frame of
        Strike ->
            if (whichFrame <= 10) then
                PostAndPostPostStrike
            else
                PostSpare

        Spare _ ->
            PostSpare

        Open _ _ ->
            PostOpen

        Partial _ ->
            PostOpen


scoreFold : Frame -> ( Mode, Score, Int ) -> ( Mode, Score, Int )
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

            PostAndPostPostStrike ->
                ( computePostStrikeMode ( frame, whichFrame ), basePoints + 2 * (naivePoints frame), nextFrame )


score : Throws -> Score
score throws =
    let
        ( finalMode, finalScore, finalFrame ) =
            List.foldl scoreFold ( PostOpen, 0, 1 ) (frameify throws)
    in
        finalScore


partitionBy : (List a -> Bool) -> Transducer a (List a) r (List a)
partitionBy pred =
    { init =
        \reduce r -> ( [], r )
    , step =
        \reduce input ( hold, r ) ->
            let
                merged =
                    hold ++ [ input ]
            in
                if (pred merged) then
                    Debug.log "true: " ( [], reduce merged r )
                else
                    Debug.log "false: " ( merged, r )
    , complete =
        \reduce ( hold, r ) ->
            if (List.isEmpty hold) then
                r
            else
                reduce hold r
    }


frameify : Throws -> List Frame
frameify throws =
    let
        completeFrame : List Int -> Bool
        completeFrame throws =
            (List.length (Debug.log "completeFrame" throws) >= 2)
                || ((List.foldl (+) 0 throws) >= 10)

        listToFrame : List Int -> Frame
        listToFrame throws =
            case List.head throws of
                Nothing ->
                    Partial -1

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
    in
        transduceList (partitionBy completeFrame) (List.reverse throws)
            |> List.reverse
            |> List.map listToFrame


frameifyOld : Throws -> List Frame
frameifyOld throws =
    let
        ( remainingThrows, computedFrame ) =
            frameHelper ( throws, Array.empty )
    in
        Array.toList computedFrame
