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
        foldWordToCount ( index, val ) dict =
            Dict.insert val
                (1 + Maybe.withDefault 0 (Dict.get val dict))
                dict

        wordToCount : WordToCount
        wordToCount =
            List.foldl foldWordToCount Dict.empty (Array.toIndexedList array)

        foldLengthToWTS : ( String, Int ) -> Dict Int (List String) -> Dict Int (List String)
        foldLengthToWTS ( val, count ) dict =
            Dict.insert count
                (val :: Maybe.withDefault [] (Dict.get count dict))
                dict

        freqToWordToCount : Dict Int (List String)
        freqToWordToCount =
            List.foldl foldLengthToWTS Dict.empty (List.reverse (Dict.toList wordToCount))
    in
        ( wordToCount, freqToWordToCount )
