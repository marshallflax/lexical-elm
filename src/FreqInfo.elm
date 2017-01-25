module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (zipLists)


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
        accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
        accumulateMaybe default verb maybe =
            Maybe.withDefault default maybe |> verb |> Just

        -- first, compute lists omitting 0, 1, 2, ..., len-1
        -- then: zip them together to get (0, 1, ..., len-1), (1, 2, ... len), (2, 3, ..., len+1) ...
        -- then: join them using the perhapsIntersperse
        -- then: count instances of each element into dict of {element -> count}
        -- then: convert to list of (element, count) pairs
        -- then: convert to dict of {count -> list element} (using foldr so the :: in the foldr does what we want)
        computeFrequencies : Int -> Int -> LenInfo
        computeFrequencies len drop =
            List.map (wordList |> flip List.drop) (List.range 0 (len - 1))
                |> Misc.zipLists
                |> List.map (perhapsIntersperse >> List.foldr (++) "")
                |> List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty
                |> Dict.toList
                |> List.foldr (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty
                |> (List.foldl Dict.remove |> flip) (List.range 1 drop)

        addNGram : ( Int, Int ) -> Dict Int LenInfo -> Dict Int LenInfo
        addNGram ( len, drop ) =
            computeFrequencies len drop |> Dict.insert len
    in
        List.foldl addNGram Dict.empty desiredLengthsAndMinimumFrequencies
