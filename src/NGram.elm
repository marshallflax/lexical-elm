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
            Dict.insert val (1 + Maybe.withDefault 0 (Dict.get val dict)) dict

        foldLengthToWTS : ( String, Int ) -> Dict Int (List String) -> Dict Int (List String)
        foldLengthToWTS ( val, count ) dict =
            let
                newWordToCount : List String
                newWordToCount =
                    case
                        Dict.get count dict
                    of
                        Nothing ->
                            [ val ]

                        Just wordToCount ->
                            val :: wordToCount
            in
                Dict.insert count newWordToCount dict

        wordToCount : WordToCount
        wordToCount =
            List.foldl foldWordToCount Dict.empty (Array.toIndexedList array)

        freqToWordToCount : Dict Int (List String)
        freqToWordToCount =
            List.foldl foldLengthToWTS Dict.empty (List.reverse (Dict.toList wordToCount))
    in
        ( wordToCount, freqToWordToCount )
