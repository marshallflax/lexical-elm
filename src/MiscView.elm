module MiscView exposing (onShiftedMouseEnter, onUnShiftedMouseEnter)

import Html
import Html.Events
import Json.Decode as Decode


forBooleanModifier : String -> String -> (Bool -> Bool) -> msg -> Html.Attribute msg
forBooleanModifier modifier eventName modifierCriteria message =
    let
        extractModfier : Decode.Decoder Bool
        extractModfier =
            Decode.at [ modifier ] Decode.bool

        applyTest : (modifier -> Bool) -> msg -> modifier -> Decode.Decoder msg
        applyTest modifierTest message modifierValue =
            if (modifierTest modifierValue) then
                Decode.succeed message
            else
                Decode.fail ("Not relevant")

        matches : Bool -> Decode.Decoder msg
        matches =
            applyTest modifierCriteria message
    in
        extractModfier |> Decode.andThen matches |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" identity


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" not
