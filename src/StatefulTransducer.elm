module StatefulTransducer exposing (..)

import Transducer exposing (Reducer, Transducer, transduce)


statefulPartitionBy : (List a -> Bool) -> Transducer a (List a) r (List a)
statefulPartitionBy predicate =
    let
        doReduce : Reducer (List a) b -> Reducer (List a) b
        doReduce reduce list =
            reduce (List.reverse list)

        doAppend : a -> List a -> List a
        doAppend input state =
            input :: state
    in
        { init =
            \reduce r -> ( [], r )
        , step =
            \reduce input ( state, currentReduction ) ->
                let
                    merged =
                        doAppend input state
                in
                    if (predicate merged) then
                        ( [], doReduce reduce merged currentReduction )
                    else
                        ( merged, currentReduction )
        , complete =
            \reduce ( state, currentReduction ) ->
                if (List.isEmpty state) then
                    currentReduction
                else
                    doReduce reduce state currentReduction
        }


{-|
   we do not do "transduce List.foldr (::) [] xform list" since we want to statefully partition from the left
-}
transduceListL : Transducer a b (List b) s -> List a -> List b
transduceListL xform list =
    let
        fold : (a -> x -> x) -> x -> List a -> x
        fold =
            List.foldl

        aggregator : b -> List b -> List b
        aggregator =
            (::)

        initialAggregationState : List b
        initialAggregationState =
            []

        transducer : Transducer a b (List b) s -> List a -> List b
        transducer =
            transduce fold aggregator initialAggregationState
    in
        transducer xform list |> List.reverse
