module ColoredWordView exposing (..)

import ColoredWord exposing (..)
import Html exposing (Html, button, div, span, text, input, p, table, tr, td)
import Html.Attributes exposing (style, value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Set exposing (Set)
import Types exposing (..)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    style [ ( "backgroundColor", colorName ) ]


colorStyles : Set String -> ColoredWord -> ColoredWord -> Html.Attribute msg
colorStyles excludeSet coloredWord currentWord =
    let
        colorSet =
            Set.diff coloredWord.colors excludeSet

        size =
            Set.size colorSet

        matchingStyle =
            if (coloredWord.normalized == currentWord.normalized) then
                [ ( "borderStyle", "solid" ), ( "borderColor", "black" ) ]
            else
                [ ( "borderStyle", "solid" ), ( "borderColor", "transparent" ) ]
    in
        if (size == 0) then
            style matchingStyle
        else if (size <= 1) then
            style (( "backgroundColor", String.join "," (Set.toList colorSet) ) :: matchingStyle)
        else
            style (( "background", "linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) :: matchingStyle)


renderWord : Set String -> String -> ColoredWord -> ( Int, ColoredWord ) -> Html Msg
renderWord hideColors currentColor currentWord ( index, w ) =
    span
        [ colorStyles hideColors w currentWord
        , onClick (ToggleColor index currentColor)
        , onMouseEnter (SetCurrentWord index)
        ]
        [ text (" " ++ w.text ++ " ") ]
