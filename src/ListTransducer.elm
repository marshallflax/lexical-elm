module ListTransducer exposing (..)

import Transducer exposing (..)


partitionBy : (List a -> Bool) -> Transducer a (List a) r (List a)
partitionBy predicate =
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
   we do not do "transduce List.foldr (::) [] xform list" since we want to partition from the left
-}
transduceListL : Transducer a b (List b) s -> List a -> List b
transduceListL xform list =
    (transduce List.foldl (::) []) xform list |> List.reverse
