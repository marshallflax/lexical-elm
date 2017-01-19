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


viewMaybeDraggable : String -> Maybe DraggableWidget -> Html Msg -> Html Msg
viewMaybeDraggable key maybeDraggable html =
    let
        onMouseDown : Attribute Msg
        onMouseDown =
            Html.Events.on "mousedown"
                (Decode.map ((DragMessage key) << DragStart) Mouse.position)
    in
        case maybeDraggable of
            Nothing ->
                div
                    [ onMouseDown ]
                    [ html ]

            Just draggable ->
                let
                    position : Position
                    position =
                        case draggable.drag of
                            Nothing ->
                                draggable.position

                            Just { start, current } ->
                                Position
                                    (draggable.position.x + current.x - start.x)
                                    (draggable.position.y + current.y - start.y)
                in
                    div
                        [ onMouseDown
                        , style
                            [ ( "position", "absolute" )
                            , ( "left", px (.x position) )
                            , ( "top", px (.y position) )
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
