module DragController exposing (doCmd)

import Mouse exposing (Position)
import Types exposing (DraggableWidget, DragState, DragCmd(..))


doCmd : DragCmd -> DraggableWidget -> DraggableWidget
doCmd cmd widget =
    case cmd of
        DragStart xy ->
            { widget
                | drag = Just (DragState xy xy)
            }

        DragAt xy ->
            { widget
                | drag = (Maybe.map (\{ start } -> DragState start xy) widget.drag)
            }

        DragEnd xy ->
            { widget
                | position =
                    case widget.drag of
                        Nothing ->
                            widget.position

                        Just { start, current } ->
                            Position
                                (widget.position.x + current.x - start.x)
                                (widget.position.y + current.y - start.y)
                , drag = Nothing
            }
