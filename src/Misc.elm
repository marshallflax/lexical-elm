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



-- ListHelper : ( List (List a), List (List a) ) -> ( List (List a), List (List a) )
-- Start with a list of lists: ((1, 2, 3), (10, 20, 30))
-- Return the transpose: ((1,10), (2, 20), (3, 30))


zipLists : List (List a) -> List (List a)
zipLists lists =
    let
        zipListHelper ( remainingInputs, outputSoFar ) =
            case
                List.foldr
                    (Maybe.map2 ((::)))
                    (Just [])
                    (List.map List.head remainingInputs)
            of
                Nothing ->
                    ( remainingInputs, outputSoFar )

                Just heads ->
                    zipListHelper
                        ( List.map (List.drop 1) remainingInputs
                        , heads :: outputSoFar
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
        List.map doShift shifts |> zipLists
