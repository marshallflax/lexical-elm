module Tests exposing (..)

import BowlingScore exposing (scoreList)
import Expect
import Fuzz exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Single Throw" <|
                \() ->
                    [ 1 ] |> scoreList |> Expect.equal 1
            , test "Two Throws" <|
                \() ->
                    [ 1, 1 ] |> scoreList |> Expect.equal 2
            , test "Spare" <|
                \() ->
                    [ 10, 1 ] |> scoreList |> Expect.equal 12
            , test "Perfect" <|
                \() ->
                    List.repeat 12 10 |> scoreList |> Expect.equal 300
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (intRange 1 10) "Single throw" <|
                \throw ->
                    [ throw ] |> scoreList |> Expect.equal throw
            , fuzz2 (intRange 1 9) (intRange 1 10) "Repeated open frames" <|
                \throw frames ->
                    List.repeat frames [ throw, 0 ] |> List.concat |> scoreList |> Expect.equal (throw * frames)
            , fuzz2 (intRange 1 9) (intRange 1 10) "Repeated open frames" <|
                \throw frames ->
                    List.repeat frames [ 0, throw ] |> List.concat |> scoreList |> Expect.equal (throw * frames)
            ]
        ]
