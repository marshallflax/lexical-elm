module Misc exposing (..)

import Set exposing (Set)


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        Set.remove element set
    else
        Set.insert element set


combineSubscriptions : List (m -> Sub msg) -> (m -> Sub msg)
combineSubscriptions list model =
    Sub.batch (List.map ((|>) model) list)


zipListHelper : ( List (List a), List (List a) ) -> ( List (List a), List (List a) )
zipListHelper ( inputs, output ) =
    case
        List.foldr
            (Maybe.map2 ((::)))
            (Just [])
            (List.map List.head inputs)
    of
        Nothing ->
            ( [], output )

        Just heads ->
            zipListHelper ( List.map (List.drop 1) inputs, heads :: output )


zipLists : List (List a) -> List (List a)
zipLists lists =
    zipListHelper ( lists, [] ) |> Tuple.second |> List.reverse
