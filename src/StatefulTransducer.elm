module StatefulTransducer exposing (statefulPartitionBy)

import Array exposing (Array, push)
import Transducer exposing (Reducer, Transducer, transduce)
import Tuple


type alias StateHolder a =
    ( Array a, ( Array a, Int ) )


pushState : a -> StateHolder a -> StateHolder a
pushState input ( state, ( remainder, count ) ) =
    ( Array.push input state, ( remainder, count ) )


emptyState : Int -> StateHolder a
emptyState count =
    ( Array.empty, ( Array.empty, count ) )


nextEmptyState : StateHolder a -> StateHolder a
nextEmptyState ( state, ( remainder, count ) ) =
    ( Array.empty, ( remainder, count + 1 ) )


statefulPartitionBy :
    (Array a -> Bool)
    -> (( Array a, Int ) -> Bool)
    -> Transducer a (StateHolder a) r (StateHolder a)
statefulPartitionBy timeForNextChunk noMoreChunks =
    let
        init : Reducer b r -> r -> ( StateHolder a, r )
        init reduce r =
            ( emptyState 0, r )

        step : Reducer (StateHolder a) r -> Reducer a ( StateHolder a, r )
        step reduce input ( state, currentReduction ) =
            if (noMoreChunks (Tuple.second state)) then
                ( state, currentReduction )
            else
                let
                    merged =
                        pushState input state
                in
                    if (timeForNextChunk (Tuple.first merged)) then
                        ( nextEmptyState state, reduce merged currentReduction )
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
