module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (dictToListL)


{-|
length_of_sequence -> frequency_of_occurrence -> list_of_occurrences
-}
type alias FreqInfo =
    Dict Int (Dict Int (List String))


empty : FreqInfo
empty =
    Dict.empty


pairedList : Bool -> Int -> List String -> List String
pairedList interposeUnderscores widths wordList =
    let
        listOfLists : List (List String)
        listOfLists =
            List.map
                (wordList |> flip List.drop)
                (List.range 0 (widths - 1))

        perhapsInterperse : List String -> List String
        perhapsInterperse =
            if (interposeUnderscores) then
                List.intersperse "_"
            else
                identity
    in
        listOfLists |> Misc.zipLists |> List.map (perhapsInterperse >> List.foldr (++) "")


countFreq : Bool -> List ( Int, Int ) -> List String -> FreqInfo
countFreq interposeUnderscores desired wordList =
    let
        accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
        accumulateMaybe default verb maybe =
            Maybe.withDefault default maybe |> verb |> Just

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
            pairedList interposeUnderscores len wordList |> countList

        multiRemove : List comparable -> Dict comparable v -> Dict comparable v
        multiRemove list dict =
            List.foldl (\k -> Dict.remove k) dict list

        addNGram : ( Int, Int ) -> Dict Int (Dict Int (List String)) -> Dict Int (Dict Int (List String))
        addNGram ( len, drop ) =
            Dict.insert len (computeFrequencies len |> multiRemove (List.range 1 drop))
    in
        List.foldl addNGram Dict.empty desired
