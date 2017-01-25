module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (zipLists, accumulateMaybe)


type alias LenInfo =
    Dict Int (List String)


{-|
length_of_sequence -> frequency_of_occurrence -> list_of_occurrences
-}
type alias FreqInfo =
    Dict Int LenInfo


empty : FreqInfo
empty =
    Dict.empty


countFreq : (List String -> List String) -> List ( Int, Int ) -> List String -> FreqInfo
countFreq perhapsIntersperse desiredLengthsAndMinimumFrequencies wordList =
    let
        addNGram : ( Int, Int ) -> Dict Int LenInfo -> Dict Int LenInfo
        addNGram ( len, drop ) =
            let
                -- compute lists omitting 0, 1, 2, ..., len-1, zip them together to get (0, 1, ..., len-1), (1, 2, ... len), (2, 3, ..., len+1) ..., then join them using the perhapsIntersperse, then count instances of each element into dict of {element -> count}
                wordToCount =
                    List.map (wordList |> flip List.drop) (List.range 0 (len - 1))
                        |> Misc.zipLists
                        |> List.map (perhapsIntersperse >> List.foldr (++) "")
                        |> List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty

                -- convert to list of (element, count) pairs, convert to dict of {count -> list element} (using foldr so the :: in the foldr does what we want)
                computeFrequencies : LenInfo
                computeFrequencies =
                    wordToCount
                        |> Dict.toList
                        |> List.foldr (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty
                        |> (List.foldl Dict.remove |> flip) (List.range 1 drop)
            in
                Dict.insert len computeFrequencies
    in
        List.foldl addNGram Dict.empty desiredLengthsAndMinimumFrequencies
