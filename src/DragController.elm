module DragController exposing (doCmd)

import Mouse exposing (Position)
import Types exposing (DraggableWidget, DragState, DragCmd(..))


doCmd : DragCmd -> DraggableWidget -> DraggableWidget
doCmd cmd ({ position, drag } as model) =
    case cmd of
        DragStart xy ->
            { model
                | position = position
                , drag = Just (DragState xy xy)
            }

        DragAt xy ->
            { model
                | position = position
                , drag = (Maybe.map (\{ start } -> DragState start xy) drag)
            }

        DragEnd xy ->
            { model
                | position =
                    case drag of
                        Nothing ->
                            position

                        Just { start, current } ->
                            Position (position.x + current.x - start.x) (position.y + current.y - start.y)
                , drag = Nothing
            }
