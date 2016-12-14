module ViewUtil exposing (onShiftedMouseEnter)

import Json.Decode
import Html.Events exposing (on)
import Html exposing (Attribute)


onModifiedEvent : String -> String -> msg -> Attribute msg
onModifiedEvent modifier eventName message =
    let
        hasModifier : Bool -> Json.Decode.Decoder msg
        hasModifier bool =
            if bool then
                Json.Decode.succeed message
            else
                Json.Decode.fail ("No " ++ modifier)
    in
        Json.Decode.at [ modifier ] Json.Decode.bool
            |> Json.Decode.andThen hasModifier
            |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Attribute msg
onShiftedMouseEnter =
    onModifiedEvent "shiftKey" "mouseenter"
