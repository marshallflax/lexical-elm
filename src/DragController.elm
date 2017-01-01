module DragController exposing (..)

import Mouse exposing (Position)
import Types exposing (..)


do : DragCmd -> Draggable -> Draggable
do cmd ({ position, drag } as model) =
    case cmd of
        DragStart xy ->
            { model | position = position, drag = Just (DragState xy xy) }

        DragAt xy ->
            { model | position = position, drag = (Maybe.map (\{ start } -> DragState start xy) drag) }

        DragEnd xy ->
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
