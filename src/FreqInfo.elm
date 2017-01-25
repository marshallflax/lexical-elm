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


countFreq : Bool -> List ( Int, Int ) -> List String -> FreqInfo
countFreq interposeUnderscores desired wordList =
    let
        accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
        accumulateMaybe default verb maybe =
            Maybe.withDefault default maybe |> verb |> Just

        -- Same as Dict.toList except uses foldl rather than foldr to get list from end, which is useful if piped into a List.foldl
        dictToListL : Dict comparable v -> List ( comparable, v )
        dictToListL dict =
            Dict.foldl (\key value list -> ( key, value ) :: list) [] dict

        -- first: count instances of each element into dict of {element -> count}
        -- then: convert to list of (element, count) pairs
        -- then: convert to dict of {count -> list element}
        countList : List comparable -> Dict Int (List comparable)
        countList list =
            list
                |> List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty
                |> dictToListL
                |> List.foldl (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty

        perhapsInterperse : Bool -> List String -> List String
        perhapsInterperse interposeUnderscores =
            if (interposeUnderscores) then
                List.intersperse "_"
            else
                identity

        computeFrequencies : Int -> Dict Int (List String)
        computeFrequencies len =
            List.map (wordList |> flip List.drop) (List.range 0 (len - 1))
                |> Misc.zipLists
                |> List.map (perhapsInterperse interposeUnderscores >> List.foldr (++) "")
                |> countList

        multiRemove : List comparable -> Dict comparable v -> Dict comparable v
        multiRemove =
            List.foldl Dict.remove |> flip

        addNGram : ( Int, Int ) -> Dict Int (Dict Int (List String)) -> Dict Int (Dict Int (List String))
        addNGram ( len, drop ) =
            Dict.insert len (computeFrequencies len |> multiRemove (List.range 1 drop))
    in
        List.foldl addNGram Dict.empty desired
