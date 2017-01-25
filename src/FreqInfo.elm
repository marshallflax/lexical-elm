module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (zipLists)


{-|
length_of_sequence -> frequency_of_occurrence -> list_of_occurrences
-}
type alias FreqInfo =
    Dict Int (Dict Int (List String))


empty : FreqInfo
empty =
    Dict.empty


countFreq : (List String -> List String) -> List ( Int, Int ) -> List String -> FreqInfo
countFreq perhapsIntersperse desiredLengthsAndMinimumFrequencies wordList =
    let
        accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
        accumulateMaybe default verb maybe =
            Maybe.withDefault default maybe |> verb |> Just

        -- Same as Dict.toList except uses foldl rather than foldr to get list from end, which is useful if piped into a List.foldl
        dictToListL : Dict comparable v -> List ( comparable, v )
        dictToListL =
            Dict.foldl (\key value list -> ( key, value ) :: list) []

        -- first: count instances of each element into dict of {element -> count}
        -- then: convert to list of (element, count) pairs
        -- then: convert to dict of {count -> list element}
        countList : List comparable -> Dict Int (List comparable)
        countList list =
            list
                |> List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty
                |> dictToListL
                |> List.foldl (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty

        computeFrequencies : Int -> Dict Int (List String)
        computeFrequencies len =
            List.map (wordList |> flip List.drop) (List.range 0 (len - 1))
                |> Misc.zipLists
                |> List.map (perhapsIntersperse >> List.foldr (++) "")
                |> countList

        addNGram : ( Int, Int ) -> Dict Int (Dict Int (List String)) -> Dict Int (Dict Int (List String))
        addNGram ( len, drop ) =
            Dict.insert len
                (computeFrequencies len |> (List.foldl Dict.remove |> flip) (List.range 1 drop))
    in
        List.foldl addNGram Dict.empty desiredLengthsAndMinimumFrequencies
