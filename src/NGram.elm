module NGram exposing (..)

import Array exposing (..)
import Dict exposing (..)
import List exposing (..)


type alias FreqInfo =
    { words :
        Dict Int (List String)
    , n2 :
        Dict Int (List String)
        -- n-grams stored as space-delimited strings, so i can index by 'em
    }


empty : FreqInfo
empty =
    { words = Dict.empty, n2 = Dict.empty }


countList : List String -> Dict Int (List String)
countList array =
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
            List.foldl foldWordToCount Dict.empty array

        freqToWordToCount : Dict Int (List String)
        freqToWordToCount =
            List.foldl
                foldLengthToWTS
                Dict.empty
                (List.reverse (Dict.toList wordToCount))
    in
        freqToWordToCount


countFreq : Array String -> FreqInfo
countFreq array =
    let
        wordList : List String
        wordList =
            Array.toList array

        shiftedList : List String
        shiftedList =
            List.drop 1 wordList

        conc : String -> String -> String
        conc a b =
            a ++ " " ++ b

        pairedList : List String
        pairedList =
            List.map2 conc wordList shiftedList
    in
        { words = countList wordList, n2 = countList pairedList }
