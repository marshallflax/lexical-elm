module StatefulTransducer exposing (..)

import Array exposing (Array, push)
import Transducer exposing (Reducer, Transducer, transduce)


statefulPartitionBy : (Array a -> Bool) -> Transducer a (Array a) r (Array a)
statefulPartitionBy predicate =
    let
        updateState : a -> Array a -> Array a
        updateState =
            Array.push

        -- init : Reducer b r -> r -> ( state, r )
        init : Reducer b r -> r -> ( Array a, r )
        init reduce r =
            ( Array.empty, r )

        -- step : Reducer b r -> Reducer a ( state, r )
        step : Reducer (Array a) r -> Reducer a ( Array a, r )
        step reduce input ( state, currentReduction ) =
            let
                merged =
                    updateState input state
            in
                if (predicate merged) then
                    ( Array.empty, reduce merged currentReduction )
                else
                    ( merged, currentReduction )

        -- complete : Reducer b r -> ( state, r ) -> r
        complete : Reducer (Array a) r -> ( Array a, r ) -> r
        complete reduce ( state, currentReduction ) =
            if (Array.isEmpty state) then
                currentReduction
            else
                reduce state currentReduction
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
