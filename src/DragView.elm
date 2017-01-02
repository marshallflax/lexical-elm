module DragView exposing (dragSubscriptions, viewDraggables)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Types exposing (Model, Draggable, Msg(DragMessage), DragCmd(DragStart, DragAt, DragEnd))


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


viewDraggables : Dict String Draggable -> Html Msg
viewDraggables draggables =
    let
        px : Int -> String
        px number =
            toString number ++ "px"

        getPosition : Draggable -> Position
        getPosition { position, drag } =
            case drag of
                Nothing ->
                    position

                Just { start, current } ->
                    Position (position.x + current.x - start.x) (position.y + current.y - start.y)

        onMouseDown : String -> Attribute Msg
        onMouseDown key =
            Html.Events.on "mousedown"
                (Decode.map (DragMessage key << DragStart) Mouse.position)

        viewDraggable : ( String, Draggable ) -> Html Msg
        viewDraggable ( key, draggable ) =
            div
                [ onMouseDown key
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
    in
        div []
            (List.map viewDraggable (Dict.toList draggables))


dragSubscriptions : Model -> Sub Msg
dragSubscriptions model =
    let
        computeSub : ( String, Draggable ) -> Sub Msg
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
