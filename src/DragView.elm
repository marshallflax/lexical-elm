module DragView exposing (dragSubscriptions, viewDraggable)

import Dict exposing (Dict)
import Html exposing (div, span, text, Html, Attribute)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Types exposing (Model, DraggableModel, IdentifiedDraggableWidget, DraggableWidget, Msg(DragMessage), DragCmd(DragStart, DragAt, DragEnd))


px : Int -> String
px number =
    toString number ++ "px"


onMouseDown : String -> Attribute Msg
onMouseDown key =
    Html.Events.on "mousedown"
        (Decode.map ((DragMessage key) << DragStart) Mouse.position)


viewDraggable : DraggableModel -> String -> Html Msg -> Html Msg
viewDraggable dict key html =
    case
        Dict.get key dict
    of
        Nothing ->
            span
                [ onMouseDown key ]
                [ html ]

        Just draggable ->
            let
                position =
                    case draggable.drag of
                        Nothing ->
                            draggable.position

                        Just { start, current } ->
                            Position (draggable.position.x + current.x - start.x) (draggable.position.y + current.y - start.y)
            in
                span
                    [ onMouseDown key
                    , style
                        [ ( "position", "relative" )
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
