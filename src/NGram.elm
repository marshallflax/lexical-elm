module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)


type alias WordToCount =
    Dict String Int

type alias FreqInfo =
    ( WordToCount, Dict Int (List String) )


empty : FreqInfo
empty =
    ( Dict.empty, Dict.empty )


countFreq : Array String -> FreqInfo
countFreq array =
    let
        foldWordToSet ( index, val ) dict =
            Dict.insert val (1 + Maybe.withDefault 0 (Dict.get val dict)) dict

        foldLengthToWTS : ( String, Int ) -> Dict Int (List String) -> Dict Int (List String)
        foldLengthToWTS ( val, setSize ) dict =
            let
                newWordToSet : List String
                newWordToSet =
                    case
                        Dict.get
                            setSize
                            dict
                    of
                        Nothing ->
                            [ val ]

                        Just wordToSet ->
                            val :: wordToSet
            in
                Dict.insert setSize newWordToSet dict

        wordToSet : WordToCount
        wordToSet =
            List.foldl foldWordToSet Dict.empty (Array.toIndexedList array)

        freqToWordToSet : Dict Int (List String)
        freqToWordToSet =
            List.foldl foldLengthToWTS Dict.empty (List.reverse (Dict.toList wordToSet))
    in
        ( wordToSet, freqToWordToSet )
