module DragView exposing (dragSubscriptions, viewMaybeDraggable)

import Html exposing (div, text, Html, Attribute)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Types exposing (Model, IdentifiedDraggableWidget, DraggableWidget, Msg(DragMessage), DragCmd(DragStart, DragAt, DragEnd))


px : Int -> String
px number =
    toString number ++ "px"


viewMaybeDraggable : Maybe IdentifiedDraggableWidget -> Html Msg -> Html Msg
viewMaybeDraggable maybeDraggable html =
    case maybeDraggable of
        Nothing ->
            html

        Just (key, draggable) ->
            let
                getPosition : DraggableWidget -> Position
                getPosition { position, drag } =
                    case drag of
                        Nothing ->
                            position

                        Just { start, current } ->
                            Position (position.x + current.x - start.x) (position.y + current.y - start.y)

                p : Position
                p = getPosition draggable

                onMouseDown : Attribute Msg
                onMouseDown =
                    Html.Events.on "mousedown"
                        (Decode.map ((DragMessage key) << DragStart) Mouse.position)
            in
                div
                    [ onMouseDown
                    , style
                        [ ( "position", "absolute" )
                        , ( "left", px (.x p) )
                        , ( "top", px (.y p) )
                        ]
                    ]
                    [ html ]


dragSubscriptions : List IdentifiedDraggableWidget -> Sub Msg
dragSubscriptions draggables =
    let
        computeSub : IdentifiedDraggableWidget -> Sub Msg
        computeSub ( key, draggable ) =
            case draggable.drag of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.batch
                        [ Mouse.moves (DragMessage key << DragAt)
                        , Mouse.ups (DragMessage key << DragEnd)
                        ]
    in
        List.map computeSub draggables |> Sub.batch
