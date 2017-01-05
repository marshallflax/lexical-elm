module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (dictToListL)


type alias FreqInfo =
    { words : Dict Int (List String)
    , n2 : Dict Int (List String)
    }


empty : FreqInfo
empty =
    { words = Dict.empty, n2 = Dict.empty }


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


pairedList : Int -> List String -> List String
pairedList widths wordList =
    List.map (wordList |> flip List.drop) (List.range 0 (widths - 1))
        |> Misc.zipLists
        |> List.map (List.intersperse "_" >> List.foldl (++) "")


countFreq : List String -> FreqInfo
countFreq wordList =
    { words =
        countList (pairedList 1 wordList)
    , n2 =
        countList (pairedList 2 wordList) |> Dict.remove 1
    }
