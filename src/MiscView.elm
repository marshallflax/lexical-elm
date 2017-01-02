module MiscView exposing (onShiftedMouseEnter, onUnShiftedMouseEnter)

import Html
import Html.Events
import Json.Decode as Decode


forBooleanModifier : Decode.Decoder a -> String -> (a -> Bool) -> msg -> Html.Attribute msg
forBooleanModifier decoder eventName modifierCriteria message =
    let
        matches : a -> Decode.Decoder msg
        matches modifierValue =
            if (modifierCriteria modifierValue) then
                Decode.succeed message
            else
                Decode.fail ("Not relevant")
    in
        decoder |> Decode.andThen matches |> Html.Events.on eventName


decodeShiftKey : Decode.Decoder Bool
decodeShiftKey =
    Decode.at [ "shiftKey" ] Decode.bool


onShiftedMouseEnter : msg -> Html.Attribute msg
onShiftedMouseEnter =
    forBooleanModifier decodeShiftKey "mouseenter" identity


onUnShiftedMouseEnter : msg -> Html.Attribute msg
onUnShiftedMouseEnter =
    forBooleanModifier decodeShiftKey "mouseenter" not
