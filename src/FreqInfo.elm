module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (dictToListL)


type alias FreqInfo =
    Dict Int (Dict Int (List String))


empty : FreqInfo
empty =
    Dict.empty


{-|
        -- first: count instances of each element into dict of {element -> count}
        -- then: convert to list of (element, count) pairs
        -- then: convert to dict of {count -> list element}
-}
countList : List comparable -> Dict Int (List comparable)
countList list =
    let
        accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
        accumulateMaybe default verb maybe =
            Maybe.withDefault default maybe |> verb |> Just
    in
        list
            |> List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty
            |> dictToListL
            |> List.foldl (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty


pairedList : Bool -> Int -> List String -> List String
pairedList interposeUnderscores widths wordList =
    let
        perhapsInterperse : List String -> List String
        perhapsInterperse =
            if (interposeUnderscores) then
                List.intersperse "_"
            else
                identity
    in
        List.map (wordList |> flip List.drop) (List.range 0 (widths - 1))
            |> Misc.zipLists
            |> List.map (perhapsInterperse >> List.foldl (++) "")


countFreq : Bool -> List ( Int, Int ) -> List String -> FreqInfo
countFreq interposeUnderscores desired wordList =
    let
        addNGram ( len, drop ) =
            Dict.insert len (countList (pairedList interposeUnderscores len wordList) |> Dict.remove drop)
    in
        List.foldl addNGram Dict.empty desired
