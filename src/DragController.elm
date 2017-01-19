module DragController exposing (doCmd)

import Mouse exposing (Position)
import Types exposing (DraggableWidget, DragState, DragCmd(..))


doCmd : DragCmd -> Maybe DraggableWidget -> Maybe DraggableWidget
doCmd dragCmd maybeWidget =
    case maybeWidget of
        Nothing ->
            Just (DraggableWidget (Position 0 0) Nothing)

        Just widget ->
            case dragCmd of
                DragStart xy ->
                    Just
                        { widget
                            | drag = Just (DragState xy xy)
                        }

                DragAt xy ->
                    Just
                        { widget
                            | drag = (Maybe.map (\{ start } -> DragState start xy) widget.drag)
                        }

                DragEnd xy ->
                    Just
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
