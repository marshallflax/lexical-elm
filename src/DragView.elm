module DragView exposing (dragSubscriptions, viewDraggables)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Types exposing (Model, DraggableWidget, Msg(DragMessage), DragCmd(DragStart, DragAt, DragEnd))


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


px : Int -> String
px number =
    toString number ++ "px"


viewDraggable : ( String, DraggableWidget ) -> Html Msg
viewDraggable ( key, draggable ) =
    let
        dcm : DragCmd -> Msg
        dcm =
            DragMessage key

        html : Html Msg
        html =
            div
                [ style
                    [ "background-color" => "#3C8D2F"
                    , "cursor" => "move"
                    , "width" => "100px"
                    , "height" => "100px"
                    , "border-radius" => "4px"
                    , "color" => "white"
                    , "align-items" => "center"
                    , "justify-content" => "center"
                    , "display" => "flex"
                    ]
                ]
                [ text key ]
    in
        viewDraggableHtml dcm draggable html


viewDraggableHtml : (DragCmd -> Msg) -> DraggableWidget -> Html Msg -> Html Msg
viewDraggableHtml dcm draggable html =
    let
        getPosition : DraggableWidget -> Position
        getPosition { position, drag } =
            case drag of
                Nothing ->
                    position

                Just { start, current } ->
                    Position (position.x + current.x - start.x) (position.y + current.y - start.y)

        onMouseDown : Attribute Msg
        onMouseDown =
            Html.Events.on "mousedown"
                (Decode.map (dcm << DragStart) Mouse.position)
    in
        div
            [ onMouseDown
            , style
                [ "position" => "absolute"
                , "left" => px (.x (getPosition draggable))
                , "top" => px (.y (getPosition draggable))
                ]
            ]
            [ html ]


viewDraggables : Dict String DraggableWidget -> Html Msg
viewDraggables draggables =
    div [] (List.map viewDraggable (Dict.toList draggables))


dragSubscriptions : Model -> Sub Msg
dragSubscriptions model =
    let
        computeSub : ( String, DraggableWidget ) -> Sub Msg
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
        Sub.batch (List.map computeSub (Dict.toList model.draggables))
