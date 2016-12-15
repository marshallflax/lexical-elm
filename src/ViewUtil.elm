module ViewUtil exposing (onShiftedMouseEnter, onUnShiftedMouseEnter)

import Html
import Html.Events
import Json.Decode


applyTest : (modifier -> Bool) -> msg -> modifier -> Json.Decode.Decoder msg
applyTest modifierTest message modifierValue =
    if (modifierTest modifierValue) then
        Json.Decode.succeed message
    else
        Json.Decode.fail ("Not relevant")


onModifiedEvent : String -> String -> (Bool -> Bool) -> msg -> Html.Attribute msg
onModifiedEvent modifier eventName modifierCriteria message =
    Json.Decode.at [ modifier ] Json.Decode.bool
        |> Json.Decode.andThen (applyTest modifierCriteria message)
        |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    onModifiedEvent "shiftKey" "mouseenter" identity


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    onModifiedEvent "shiftKey" "mouseenter" not
