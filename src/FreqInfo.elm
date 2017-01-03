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
        foldWordToCount : comparable -> Dict comparable Int -> Dict comparable Int
        foldWordToCount val dict =
            Dict.insert val
                (1 + Maybe.withDefault 0 (Dict.get val dict))
                dict

        foldLengthToWTS : ( comparable, Int ) -> Dict Int (List comparable) -> Dict Int (List comparable)
        foldLengthToWTS ( val, count ) dict =
            Dict.insert count
                (val :: Maybe.withDefault [] (Dict.get count dict))
                dict
    in
        List.foldl foldWordToCount Dict.empty list
            |> Dict.toList
            |> List.reverse
            |> List.foldl foldLengthToWTS Dict.empty


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
