module FreqInfo exposing (FreqInfo, empty, countFreq)

import Dict exposing (Dict)


type alias FreqInfo =
    { words :
        Dict Int (List String)
    , n2 :
        Dict Int (List String)
        -- n-grams stored as underscore-delimited strings, so i can index by 'em
    }


empty : FreqInfo
empty =
    { words = Dict.empty, n2 = Dict.empty }


{-| Same as Dict.toList except uses foldl rather than foldr to get list from end, which is useful if piped into a List.foldl
-}
dictToListL : Dict comparable v -> List ( comparable, v )
dictToListL dict =
    Dict.foldl (\key value list -> ( key, value ) :: list) [] dict


accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
accumulateMaybe default verb maybe =
    Maybe.withDefault default maybe |> verb |> Just


countList : List comparable -> Dict Int (List comparable)
countList list =
    list
        -- count instances of each element into dict of {element -> count}
        |>
            List.foldl (flip Dict.update (accumulateMaybe 0 ((+) 1))) Dict.empty
        -- convert to list of (element, count) pairs
        |>
            dictToListL
        -- convert to dict of {count -> list element}
        |>
            List.foldl (\( val, count ) -> Dict.update count (accumulateMaybe [] ((::) val))) Dict.empty


pairedList : (String -> String -> String) -> List String -> List String
pairedList conc wordList =
    List.map2 conc wordList (List.drop 1 wordList)


conc : String -> String -> String
conc a b =
    a ++ "_" ++ b


countFreq : List String -> FreqInfo
countFreq wordList =
    { words =
        countList wordList
    , n2 =
        -- remove singleton 2-grams
        Dict.remove 1 (countList (pairedList conc wordList))
    }
