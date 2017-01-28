module ColoredWordView exposing (colorStyle, matchingStyle, renderWord)

import ColoredWord exposing (ColoredWord)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import MiscView
import Set exposing (Set)
import Styles
import Types exposing (..)


colorStyle : String -> Html.Attribute msg
colorStyle colorName =
    Html.Attributes.style [ ( "backgroundColor", colorName ) ]


matchingStyle : Bool -> Html.Attribute msg
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

        matchStyle =
            matchingStyle <| Set.member coloredWord.normalized currentWordsNormalized
    in
        if (size == 0) then
            [ matchStyle ]
        else if (size <= 1) then
            [ Html.Attributes.style [ ( "backgroundColor", String.join "," (Set.toList colorSet) ) ]
            , matchStyle
            ]
        else
            [ Html.Attributes.style [ ( "background", "linear-gradient(90deg," ++ String.join "," (Set.toList colorSet) ++ ")" ) ]
            , matchStyle
            ]


renderWord : Set String -> String -> Set String -> ( Int, ColoredWord ) -> Html Msg
renderWord hideColors currentColor currentWordsNormalized ( index, w ) =
    Html.span
        (onClick (LexicalMessage (ToggleColor index currentColor))
            :: MiscView.onShiftedMouseEnter (LexicalMessage (SetCurrentWord index))
            :: colorStyles hideColors w currentWordsNormalized
        )
        [ Html.text (" " ++ w.text ++ " ") ]
