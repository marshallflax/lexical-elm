module ColoredWordView exposing (..)

import ColoredWord exposing (..)
import Html exposing (..)
import Html.Attributes exposing (value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (on, onClick, onInput, onMouseEnter)
import Set exposing (Set)
import Types exposing (..)
import ViewUtil exposing (..)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    Html.Attributes.style [ ( "backgroundColor", colorName ) ]


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
            Html.Attributes.style (matchingStyle isMatch)
        else if (size <= 1) then
            Html.Attributes.style (( "backgroundColor", String.join "," (Set.toList colorSet) ) :: (matchingStyle isMatch))
        else
            Html.Attributes.style (( "background", "linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) :: (matchingStyle isMatch))


renderWord : Set String -> String -> Set String -> ( Int, ColoredWord ) -> Html Msg
renderWord hideColors currentColor currentWordsNormalized ( index, w ) =
    span
        [ colorStyles hideColors w currentWordsNormalized
        , onClick (ToggleColor index currentColor)
        , onShiftedMouseEnter (LexicalMessage (SetCurrentWord index))
        ]
        [ text (" " ++ w.text ++ " ") ]
