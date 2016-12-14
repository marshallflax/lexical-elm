module ViewUtil exposing (onShiftedMouseEnter)

import Json.Decode
import Html.Events
import Html


onBooleanEvent : String -> String -> (Bool -> Json.Decode.Decoder msg) -> Html.Attribute msg
onBooleanEvent modifier eventName toMsg =
    Json.Decode.at [ modifier ] Json.Decode.bool
        |> Json.Decode.andThen toMsg
        |> Html.Events.on eventName


messageIfTrue : msg -> Bool -> Json.Decode.Decoder msg
messageIfTrue message bool =
    if bool then
        Json.Decode.succeed message
    else
        Json.Decode.fail ("No match")


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter message =
    onBooleanEvent "shiftKey" "mouseenter" (messageIfTrue message)
