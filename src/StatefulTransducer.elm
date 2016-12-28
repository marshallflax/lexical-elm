module StatefulTransducer exposing (..)

import Transducer exposing (Reducer, Transducer, transduce)


statefulPartitionBy : (List a -> Bool) -> Transducer a (List a) r (List a)
statefulPartitionBy predicate =
    let
        -- doReduce : Reducer input result -> Reducer input result
        doReduce : (List a -> b -> b) -> List a -> b -> b
        doReduce reduce state currentReduction =
            reduce (List.reverse state) currentReduction

        updateState : a -> List a -> List a
        updateState input state =
            input :: state

        -- init : Reducer b r -> r -> ( state, r )
        init : Reducer b r -> r -> ( List a, r )
        init reduce r =
            ( [], r )

        -- step : Reducer b r -> Reducer a ( state, r )
        step : Reducer (List a) r -> Reducer a ( List a, r )
        step reduce input ( state, currentReduction ) =
            let
                merged =
                    updateState input state
            in
                if (predicate merged) then
                    ( [], doReduce reduce merged currentReduction )
                else
                    ( merged, currentReduction )

        -- complete : Reducer b r -> ( state, r ) -> r
        complete : Reducer (List a) r -> ( List a, r ) -> r
        complete reduce ( state, currentReduction ) =
            if (List.isEmpty state) then
                currentReduction
            else
                doReduce reduce state currentReduction
    in
        { init = init, step = step, complete = complete }


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
