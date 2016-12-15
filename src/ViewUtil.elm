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


forBooleanModifier : String -> String -> (Bool -> Bool) -> msg -> Html.Attribute msg
forBooleanModifier modifier eventName modifierCriteria message =
    Json.Decode.at [ modifier ] Json.Decode.bool
        |> Json.Decode.andThen (applyTest modifierCriteria message)
        |> Html.Events.on eventName


forStringModifier : String -> String -> (String -> Bool) -> msg -> Html.Attribute msg
forStringModifier modifier eventName modifierCriteria message =
    Json.Decode.at [ modifier ] Json.Decode.string
        |> Json.Decode.andThen (applyTest modifierCriteria message)
        |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" identity


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" not
