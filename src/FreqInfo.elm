module FreqInfo exposing (LenInfo, emptyLenInfo, FreqInfo, empty, countFreq)

import Dict exposing (Dict)
import Misc exposing (zipLists, accumulateMaybe)


type alias LenInfo =
    { tokenToCount : Dict String Int
    , freqToList : Dict Int (List String)
    }


emptyLenInfo : LenInfo
emptyLenInfo =
    { tokenToCount = Dict.empty, freqToList = Dict.empty }


type alias FreqInfo =
    Dict Int LenInfo


empty : FreqInfo
empty =
    Dict.empty


countFreq : (List String -> List String) -> List ( Int, Int ) -> List String -> FreqInfo
countFreq perhapsIntersperse desiredLengthsAndMinimumFrequencies tokenList =
    let
        addNGram : ( Int, Int ) -> Dict Int LenInfo -> Dict Int LenInfo
        addNGram ( len, drop ) =
            let
                -- compute lists omitting 0, 1, 2, ..., len-1, zip them together to get (0, 1, ..., len-1), (1, 2, ... len), (2, 3, ..., len+1) ..., then join them using the perhapsIntersperse, then count instances of each element into dict of {element -> count}
                tokenToCount : Dict String Int
                tokenToCount =
                    List.map (tokenList |> flip List.drop) (List.range 0 (len - 1))
                        |> Misc.zipLists
                        |> List.map (perhapsIntersperse >> List.foldr (++) "")
                        |> List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty

                -- convert to list of (element, count) pairs, convert to dict of {count -> list element} (using foldr so the :: in the foldr does what we want)
                freqToList : Dict Int (List String)
                freqToList =
                    tokenToCount
                        |> Dict.toList
                        |> List.foldr (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty
                        |> (List.foldl Dict.remove |> flip) (List.range 1 drop)
            in
                Dict.insert len { tokenToCount = tokenToCount, freqToList = freqToList }
    in
        List.foldl addNGram Dict.empty desiredLengthsAndMinimumFrequencies
