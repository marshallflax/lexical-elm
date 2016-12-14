module ColoredWordView exposing (..)

import ColoredWord exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (on, onClick, onInput, onMouseEnter)
import Json.Decode
import Set exposing (Set)
import Types exposing (..)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style [ ( "backgroundColor", colorName ) ]


matchingStyle : Bool -> List ( String, String )
matchingStyle matches =
    if matches then
        [ ( "borderStyle", "solid" ), ( "borderColor", "black" ) ]
    else
        [ ( "borderStyle", "solid" ), ( "borderColor", "transparent" ) ]


colorStyles : Set String -> ColoredWord -> Set String -> Html.Attribute msg
colorStyles excludeSet coloredWord currentWordsNormalized =
    let
        colorSet =
            Set.diff coloredWord.colors excludeSet

        size =
            Set.size colorSet

        isMatch =
            Set.member coloredWord.normalized currentWordsNormalized
    in
        if (size == 0) then
            style (matchingStyle isMatch)
        else if (size <= 1) then
            style (( "backgroundColor", String.join "," (Set.toList colorSet) ) :: (matchingStyle isMatch))
        else
            style (( "background", "linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) :: (matchingStyle isMatch))


renderWord : Set String -> String -> Set String -> ( Int, ColoredWord ) -> Html Msg
renderWord hideColors currentColor currentWordsNormalized ( index, w ) =
    span
        [ colorStyles hideColors w currentWordsNormalized
        , onClick (ToggleColor index currentColor)
          -- , onMouseEnter (SetCurrentWord index)
        , onShiftedEvent "mouseenter" (SetCurrentWord index)
        ]
        [ text (" " ++ w.text ++ " ") ]


onShiftedEvent : String -> msg -> Attribute msg
onShiftedEvent eventName message =
    let
        hasShift : msg -> Bool -> Json.Decode.Decoder msg
        hasShift message shiftKey =
            if shiftKey then
                Json.Decode.succeed message
            else
                Json.Decode.fail "No shift key"

        onEventName : Json.Decode.Decoder msg -> Attribute msg
        onEventName =
            Html.Events.on eventName

        extractShiftKey : Json.Decode.Decoder Bool
        extractShiftKey =
            Json.Decode.at [ "shiftKey" ] Json.Decode.bool
    in
        onEventName <|
            Json.Decode.andThen (hasShift message) <|
                extractShiftKey
