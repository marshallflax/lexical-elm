module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)
import Set exposing (..)


type alias WordToSet comparable1 =
    Dict comparable1 (Set Int)


type alias FreqInfo comparable1 =
    ( WordToSet comparable1, Dict Int (WordToSet comparable1) )


empty : FreqInfo comparable1
empty =
    ( Dict.empty, Dict.empty )


countFreq : Array comparable1 -> FreqInfo comparable1
countFreq array =
    let
        foldWordToSet : ( Int, comparable1 ) -> WordToSet comparable1 -> WordToSet comparable1
        foldWordToSet ( index, val ) dict =
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

        wordToSet =
            List.foldl foldWordToSet Dict.empty (Array.toIndexedList array)


    in
        ( wordToSet, Dict.empty )
