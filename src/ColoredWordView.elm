module ColoredWordView exposing (colorStyle, matchingStyle, renderWord)

import ColoredWord exposing (ColoredWord)
import Html exposing (Html, Attribute, span, text)
import Html.Attributes exposing (value, checked, type_, readonly, placeholder, href)
import Html.Events exposing (on, onClick, onInput, onMouseEnter)
import MiscView exposing (onShiftedMouseEnter)
import Set exposing (Set)
import Styles
import Types exposing (..)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    Html.Attributes.style [ ( "backgroundColor", colorName ) ]


matchingStyle : Bool -> Attribute msg
matchingStyle matches =
    if (matches) then
        Styles.useClass Styles.SolidBlackBorder
    else
        Styles.useClass Styles.SolidTransparentBorder


colorStyles : Set String -> ColoredWord -> Set String -> List (Html.Attribute msg)
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
            [ matchingStyle isMatch ]
        else if (size <= 1) then
            (Html.Attributes.style [ ( "backgroundColor", String.join "," (Set.toList colorSet) ) ]) :: [ matchingStyle isMatch ]
        else
            (Html.Attributes.style [ ( "background", "linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) ]) :: [ matchingStyle isMatch ]


renderWord : Set String -> String -> Set String -> ( Int, ColoredWord ) -> Html Msg
renderWord hideColors currentColor currentWordsNormalized ( index, w ) =
    span
        (onClick (LexicalMessage (ToggleColor index currentColor))
            :: onShiftedMouseEnter (LexicalMessage (SetCurrentWord index))
            :: colorStyles hideColors w currentWordsNormalized
        )
        [ text (" " ++ w.text ++ " ") ]
