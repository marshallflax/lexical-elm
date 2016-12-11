module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)
import Set exposing (..)


type alias FreqInfo comparable1 =
    Dict comparable1 (Set Int)


countFreq : Array comparable1 -> FreqInfo comparable1
countFreq array =
    let
        fold : ( Int, comparable1 ) -> FreqInfo comparable1 -> FreqInfo comparable1
        fold ( index, val ) dict =
            let
                newSet : Set Int
                newSet =
                    case
                        Dict.get val dict
                    of
                        Nothing ->
                            Set.insert index Set.empty

                        Just set ->
                            Set.insert index set
            in
                Dict.insert val newSet dict
    in
        List.foldl fold Dict.empty (Array.toIndexedList array)
