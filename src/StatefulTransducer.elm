module StatefulTransducer exposing (statefulPartitionBy)

import Array exposing (Array, push)
import Transducer exposing (Reducer, Transducer, transduce)
import Tuple


type alias StateHolder a =
    ( Array a, Array a )


pushState : a -> StateHolder a -> StateHolder a
pushState input ( state, remainder ) =
    ( Array.push input state, remainder )


statefulPartitionBy : (Array a -> Bool) -> Transducer a (StateHolder a) r (StateHolder a)
statefulPartitionBy predicate =
    let
        init : Reducer b r -> r -> ( StateHolder a, r )
        init reduce r =
            ( ( Array.empty, Array.empty ), r )

        step : Reducer (StateHolder a) r -> Reducer a ( StateHolder a, r )
        step reduce input ( state, currentReduction ) =
            let
                merged =
                    pushState input state
            in
                if (predicate (Tuple.first merged)) then
                    ( ( Array.empty, Array.empty ), reduce merged currentReduction )
                else
                    ( merged, currentReduction )

        complete : Reducer (StateHolder a) r -> ( StateHolder a, r ) -> r
        complete reduce ( state, currentReduction ) =
            if (Array.isEmpty (Tuple.first state)) then
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
