module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)
import Set exposing (..)


type alias FreqInfo a =
    Dict a (Set Int)


countFreq : Array a -> FreqInfo a
countFreq array =
    let
        fold : ( Int, a ) -> FreqInfo a -> FreqInfo a
        fold ( count, val ) dict =
            dict

        start : FreqInfo a
        start =
            Dict.empty
    in
        List.foldl fold start (Array.toIndexedList array)
