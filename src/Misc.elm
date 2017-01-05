module Misc exposing (dictToListL, toggleSet, combineSubscriptions, zipLists)

import Dict exposing (Dict)
import Set exposing (Set)


{-| Same as Dict.toList except uses foldl rather than foldr to get list from end, which is useful if piped into a List.foldl
-}
dictToListL : Dict comparable v -> List ( comparable, v )
dictToListL dict =
    Dict.foldl (\key value list -> ( key, value ) :: list) [] dict


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        Set.remove element set
    else
        Set.insert element set


combineSubscriptions : List (m -> Sub msg) -> (m -> Sub msg)
combineSubscriptions list model =
    Sub.batch (List.map ((|>) model) list)


zipLists : List (List a) -> List (List a)
zipLists lists =
    let
        zipListHelper : ( List (List a), List (List a) ) -> ( List (List a), List (List a) )
        zipListHelper ( inputs, output ) =
            case
                List.foldr
                    (Maybe.map2 ((::)))
                    (Just [])
                    (List.map List.head inputs)
            of
                Nothing ->
                    ( inputs, output )

                Just heads ->
                    zipListHelper
                        ( List.map (List.drop 1) inputs
                        , heads :: output
                        )
    in
        zipListHelper ( lists, [] )
            |> Tuple.second
            |> List.reverse
