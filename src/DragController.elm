module DragController exposing (..)

import Mouse exposing (Position)
import Types exposing (..)


do : DragVerb -> Position -> Draggable -> Draggable
do verb xy ({ position, drag } as model) =
    case verb of
        DragStart ->
            { model | position = position, drag = Just (DragState xy xy) }

        DragAt ->
            { model | position = position, drag = (Maybe.map (\{ start } -> DragState start xy) drag) }

        DragEnd ->
            let
                getPosition : Draggable -> Position
                getPosition { position, drag } =
                    case drag of
                        Nothing ->
                            position

                        Just { start, current } ->
                            Position (position.x + current.x - start.x) (position.y + current.y - start.y)
            in
                { model | position = getPosition model, drag = Nothing }
