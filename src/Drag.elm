module Drag exposing (..)

import Mouse exposing (Position)
import Types exposing (..)


doStart : Position -> Draggable -> Draggable
doStart xy ({ position, drag } as model) =
    { model | position = position, drag = Just (DragState xy xy) }


doAt : Position -> Draggable -> Draggable
doAt xy ({ position, drag } as model) =
    { model | position = position, drag = (Maybe.map (\{ start } -> DragState start xy) drag) }


doEnd : Position -> Draggable -> Draggable
doEnd xy ({ position, drag } as model) =
    { model | position = getPosition model, drag = Nothing }


getPosition : Draggable -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
