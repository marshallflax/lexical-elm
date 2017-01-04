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
                heads : List (List a) -> List (Maybe a)
                heads =
                    List.map List.head

                folder : Maybe a -> Maybe (List a) -> Maybe (List a)
                folder headElement maybeList =
                    case headElement of
                        Nothing ->
                            Nothing

                        Just elt ->
                            case maybeList of
                                Nothing ->
                                    Nothing

                                Just l ->
                                    elt :: l |> Just

                headList : List (List a) -> Maybe (List a)
                headList input =
                    List.foldl folder (Just []) (heads input)
            in
                case (headList input) of
                    Just heads ->
                        zipListHelper ( List.map (List.drop 1) input, heads :: output )

                    Nothing ->
                        ( [], output )
    in
        zipListHelper ( list, [] ) |> Tuple.second
