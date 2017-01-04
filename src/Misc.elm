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


zipLists : List (List a) -> List (List a)
zipLists list =
    let
        zipListHelper : ( List (List a), List (List a) ) -> ( List (List a), List (List a) )
        zipListHelper ( input, output ) =
            let
                --newFolder : Maybe a -> Maybe (List a) -> Maybe (List a)
                --newFolder =
                --  Maybe.map (Maybe.andThen((::)))
                folder : Maybe a -> Maybe (List a) -> Maybe (List a)
                folder headElement maybeList =
                    case headElement of
                        Nothing ->
                            Nothing

                        Just elt ->
                            Maybe.map ((::) elt) maybeList
            in
                case
                    List.foldr folder (Just []) (List.map List.head input)
                of
                    Just heads ->
                        zipListHelper ( List.map (List.drop 1) input, heads :: output )

                    Nothing ->
                        ( [], output )
    in
        zipListHelper ( list, [] ) |> Tuple.second |> List.reverse
