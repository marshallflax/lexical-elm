module MiscView exposing (onShiftedMouseEnter, onUnShiftedMouseEnter)

import Html
import Html.Events
import Json.Decode as JD


applyTest : (modifier -> Bool) -> msg -> modifier -> JD.Decoder msg
applyTest modifierTest message modifierValue =
    if (modifierTest modifierValue) then
        JD.succeed message
    else
        JD.fail ("Not relevant")


forBooleanModifier : String -> String -> (Bool -> Bool) -> msg -> Html.Attribute msg
forBooleanModifier modifier eventName modifierCriteria message =
    JD.at [ modifier ] JD.bool
        |> JD.andThen (applyTest modifierCriteria message)
        |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" identity


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" not
