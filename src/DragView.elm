module DragView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Mouse exposing (Position)
import Types exposing (..)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


getPosition : Draggable -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)


view : Draggable -> Html Msg
view draggable =
    let
        realPosition =
            getPosition draggable
    in
        div
            [ onMouseDown
            , style
                [ "background-color" => "#3C8D2F"
                , "cursor" => "move"
                , "width" => "100px"
                , "height" => "100px"
                , "border-radius" => "4px"
                , "position" => "absolute"
                , "left" => px realPosition.x
                , "top" => px realPosition.y
                , "color" => "white"
                , "display" => "flex"
                , "align-items" => "center"
                , "justify-content" => "center"
                ]
            ]
            [ text "Drag Me!"
            ]


px : Int -> String
px number =
    toString number ++ "px"


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map (Drag DragStart) Mouse.position)


dragSubscriptions : Model -> Sub Msg
dragSubscriptions model =
    case model.dragState.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves (Drag DragAt), Mouse.ups (Drag DragEnd) ]
