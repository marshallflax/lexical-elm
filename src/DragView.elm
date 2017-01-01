module DragView exposing (..)

import Dict exposing (..)
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


viewDraggables : Dict String Draggable -> Html Msg
viewDraggables draggables =
    div []
        (List.map viewDraggable (Dict.toList draggables))


viewDraggable : ( String, Draggable ) -> Html Msg
viewDraggable ( key, draggable ) =
    div
        [ onMouseDown
        , style
            [ "background-color" => "#3C8D2F"
            , "cursor" => "move"
            , "width" => "100px"
            , "height" => "100px"
            , "border-radius" => "4px"
            , "position" => "absolute"
            , "left" => px (.x (getPosition draggable))
            , "top" => px (.y (getPosition draggable))
            , "color" => "white"
            , "display" => "flex"
            , "align-items" => "center"
            , "justify-content" => "center"
            ]
        ]
        [ text key ]


px : Int -> String
px number =
    toString number ++ "px"


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map (Drag DragStart) Mouse.position)


dragSubscriptions : Model -> Sub Msg
dragSubscriptions model =
    let
        subs =
            (List.map computeSub (Dict.toList model.draggables))
    in
        Sub.batch subs


computeSub : ( String, Draggable ) -> Sub Msg
computeSub ( key, draggable ) =
    case draggable.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves (Drag DragAt), Mouse.ups (Drag DragEnd) ]
