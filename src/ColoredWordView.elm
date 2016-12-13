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
        , onMouseEnter (SetCurrentWord index)
        ]
        [ text (" " ++ w.text ++ " ") ]
