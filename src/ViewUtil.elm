module ViewUtil exposing (onShiftedMouseEnter)

import Json.Decode
import Html.Events exposing (on)
import Html exposing (Attribute)


hasModifier : msg -> Bool -> Json.Decode.Decoder msg
hasModifier message bool =
    if bool then
        Json.Decode.succeed message
    else
        Json.Decode.fail ("No match")


onModifiedEvent : String -> String -> msg -> Attribute msg
onModifiedEvent modifier eventName message =
    let
        toMsg : Bool -> Json.Decode.Decoder msg
        toMsg =
            hasModifier message
    in
        Json.Decode.at [ modifier ] Json.Decode.bool
            |> Json.Decode.andThen toMsg
            |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Attribute msg
onShiftedMouseEnter message =
    onModifiedEvent "shiftKey" "mouseenter" message
