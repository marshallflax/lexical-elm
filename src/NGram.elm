module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)
import Set exposing (..)


type alias WordToSet =
    Dict String (Set Int)


type alias FreqInfo =
    ( WordToSet, Dict Int (List String) )


empty : FreqInfo
empty =
    ( Dict.empty, Dict.empty )


countFreq : Array String -> FreqInfo
countFreq array =
    let
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

        foldLengthToWTS : ( String, Set Int ) -> Dict Int (List String) -> Dict Int (List String)
        foldLengthToWTS ( val, set ) dict =
            let
                setSize : Int
                setSize =
                    Set.size set

                newWordToSet : List String
                newWordToSet =
                    case
                        Dict.get
                            setSize
                            dict
                    of
                        Nothing ->
                            [val]

                        Just wordToSet ->
                            val :: wordToSet
            in
                Dict.insert setSize newWordToSet dict

        wordToSet : WordToSet
        wordToSet =
            List.foldl foldWordToSet Dict.empty (Array.toIndexedList array)

        freqToWordToSet : Dict Int (List String)
        freqToWordToSet =
            List.foldl foldLengthToWTS Dict.empty (Dict.toList wordToSet)
    in
        ( wordToSet, freqToWordToSet )
