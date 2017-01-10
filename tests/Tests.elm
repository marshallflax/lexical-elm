module Tests exposing (..)

import Array
import BowlingScore exposing (scoreList)
import Expect
import Fuzz exposing (..)
import Misc
import Set
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
            , fuzz3 (array int) (int) (bool) "toggleSet" <|
                \array toBeIndex novel ->
                    let
                        ( set, perhapsMember ) =
                            randomSetAndPerhapsMember array toBeIndex novel

                        toggledSet =
                            Misc.toggleSet perhapsMember set
                    in
                        Set.member perhapsMember toggledSet |> Expect.notEqual (Set.member perhapsMember set)
            , fuzz3 (array int) (int) (bool) "doubleToggleSet" <|
                \array toBeIndex novel ->
                    let
                        ( set, perhapsMember ) =
                            randomSetAndPerhapsMember array toBeIndex novel

                        doubleToggledSet =
                            Misc.toggleSet perhapsMember (Misc.toggleSet perhapsMember set)
                    in
                        Set.member perhapsMember doubleToggledSet |> Expect.equal (Set.member perhapsMember set)
            ]
        ]


randomSetAndPerhapsMember : Array.Array Int -> Int -> Bool -> ( Set.Set Int, Int )
randomSetAndPerhapsMember array toBeIndex novel =
    let
        len =
            Array.length array

        maybeMember =
            if (novel || len == 0) then
                toBeIndex
            else
                Maybe.withDefault 0 (Array.get (toBeIndex % len) array)

        set =
            Array.toList array |> Set.fromList
    in
        ( set, maybeMember )
