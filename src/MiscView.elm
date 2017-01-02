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

        matches : Bool -> Decode.Decoder msg
        matches modifierValue =
            if (modifierCriteria modifierValue) then
                Decode.succeed message
            else
                Decode.fail ("Not relevant")
    in
        extractModfier |> Decode.andThen matches |> Html.Events.on eventName


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" identity


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    forBooleanModifier "shiftKey" "mouseenter" not
