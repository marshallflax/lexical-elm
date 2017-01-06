module StatefulTransducer exposing (..)

import Array exposing (Array, push)
import Transducer exposing (Reducer, Transducer, transduce)


statefulPartitionBy : (Array a -> Bool) -> Transducer a (Array a) r (Array a)
statefulPartitionBy predicate =
    let
        init : Reducer b r -> r -> ( Array a, r )
        init reduce r =
            ( Array.empty, r )

        step : Reducer (Array a) r -> Reducer a ( Array a, r )
        step reduce input ( state, currentReduction ) =
            let
                merged =
                    Array.push input state
            in
                if (predicate merged) then
                    ( Array.empty, reduce merged currentReduction )
                else
                    ( merged, currentReduction )

        complete : Reducer (Array a) r -> ( Array a, r ) -> r
        complete reduce ( state, currentReduction ) =
            if (Array.isEmpty state) then
                currentReduction
            else
                reduce state currentReduction
    in
        { init = init, step = step, complete = complete }


{-|
   we do not do "transduce List.foldr (::) [] xform" when we want to statefully partition from the left
   (not currently in use since now we're transducing arrays which naturally do foldl's)
-}
transduceListStateful : Transducer a b (List b) s -> List a -> List b
transduceListStateful xform =
    (transduce List.foldl (::) []) xform >> List.reverse
