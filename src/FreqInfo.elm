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


countList : List comparable -> Dict Int (List comparable)
countList list =
    let
        addMaybe : Int -> Maybe Int -> Maybe Int
        addMaybe inc maybe =
            Maybe.withDefault 0 maybe |> (+) inc |> Just

        consMaybe : comparable -> Maybe (List comparable) -> Maybe (List comparable)
        consMaybe val maybe =
            Just (val :: (Maybe.withDefault [] maybe))
    in
        List.foldl (\val -> Dict.update val (addMaybe 1)) Dict.empty list
            |> Dict.toList
            |> List.reverse
            |> List.foldl (\( val, count ) -> Dict.update count (consMaybe val)) Dict.empty


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
