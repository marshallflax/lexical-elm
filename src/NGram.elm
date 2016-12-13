module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)


type alias FreqInfo =
    ( Dict String Int, Dict Int (List String) )


empty : FreqInfo
empty =
    ( Dict.empty, Dict.empty )


countFreq : Array String -> FreqInfo
countFreq array =
    let
        foldWordToCount : String -> Dict String Int -> Dict String Int
        foldWordToCount val dict =
            Dict.insert val
                (1 + Maybe.withDefault 0 (Dict.get val dict))
                dict

        foldLengthToWTS : ( String, Int ) -> Dict Int (List String) -> Dict Int (List String)
        foldLengthToWTS ( val, count ) dict =
            Dict.insert count
                (val :: Maybe.withDefault [] (Dict.get count dict))
                dict

        wordToCount : Dict String Int
        wordToCount =
            List.foldl
                foldWordToCount
                Dict.empty
                (Array.toList array)

        freqToWordToCount : Dict Int (List String)
        freqToWordToCount =
            List.foldl
                foldLengthToWTS
                Dict.empty
                (List.reverse (Dict.toList wordToCount)) -- Reverse since we build List with :: within the foldl
    in
        ( wordToCount, freqToWordToCount )
