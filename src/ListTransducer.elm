module ListTransducer exposing (..)

import Transducer exposing (..)


partitionBy : (List a -> Bool) -> Transducer a (List a) r (List a)
partitionBy pred =
    let
        doReduce : (List a -> b -> b) -> List a -> b -> b
        doReduce reduce list currentReduction =
            reduce (List.reverse list) currentReduction

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
                    if (pred merged) then
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


transduceListL : Transducer a b (List b) s -> List a -> List b
transduceListL xform list =
    (transduce List.foldl (::) []) xform list |> List.reverse
