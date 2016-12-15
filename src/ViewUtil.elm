module ViewUtil exposing (onShiftedMouseEnter)

import Json.Decode
import Html.Events
import Html


onBooleanEvent : String -> (Bool -> Json.Decode.Decoder msg) -> String -> Html.Attribute msg
onBooleanEvent modifier boolToMsg eventName =
    Json.Decode.at [ modifier ] Json.Decode.bool
        |> Json.Decode.andThen boolToMsg
        |> Html.Events.on eventName


onModifiedEvent : String -> (Bool -> Bool) -> String -> msg -> Html.Attribute msg
onModifiedEvent modifier modifierTest eventName message =
    onBooleanEvent
        modifier
        (\modifierTrue ->
            if (modifierTest modifierTrue) then
                Json.Decode.succeed message
            else
                Json.Decode.fail ("Not relevant")
        )
        eventName


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    onModifiedEvent "shiftKey" identity "mouseenter"


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    onModifiedEvent "shiftKey" not "mouseenter"
