module Misc exposing (toggleSet, combineSubscriptions, zipLists, accumulateMaybe, zipShifts)

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


accumulateMaybe : b -> (b -> a) -> Maybe b -> Maybe a
accumulateMaybe default verb maybe =
    Maybe.withDefault default maybe |> verb |> Just


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


zipShifts : a -> List Int -> List a -> List (List a)
zipShifts default shifts list =
    let
        doShift shift =
            ((List.drop shift list) ++ (List.repeat shift default))
    in
        List.map doShift shifts
