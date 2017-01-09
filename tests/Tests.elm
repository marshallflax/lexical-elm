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
            , fuzz2 (array int) (int) "toggleSet" <|
                \array toBeIndex ->
                    if (Array.length array == 0) then
                        Expect.pass
                    else
                        let
                            maybeMember =
                                Array.get (toBeIndex % (Array.length array)) array

                            set =
                                Array.toList array |> Set.fromList
                        in
                            case maybeMember of
                                Just member ->
                                    let
                                        toggledSet =
                                            Misc.toggleSet member set
                                    in
                                        Set.member member toggledSet |> Expect.notEqual (Set.member member set)

                                Nothing ->
                                    Expect.fail "Not member of list"
            , fuzz2 (array int) (int) "doubloToggleSet" <|
                \array toBeIndex ->
                    if (Array.length array == 0) then
                        Expect.pass
                    else
                        let
                            maybeMember =
                                Array.get (toBeIndex % (Array.length array)) array

                            set =
                                Array.toList array |> Set.fromList
                        in
                            case maybeMember of
                                Just member ->
                                    let
                                        doubleToggledSet =
                                            Misc.toggleSet member (Misc.toggleSet member set)
                                    in
                                        Set.member member doubleToggledSet |> Expect.equal (Set.member member set)

                                Nothing ->
                                    Expect.fail "Not member of list"
            ]
        ]
