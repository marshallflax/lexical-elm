module Styles exposing (..)

import Css
import Html


stylesheet : Css.Stylesheet Id Class msg
stylesheet =
    Css.stylesheet []
        [ { selectors = [ Css.Class NumberLineClass ], descriptor = [ ( "counter-increment", "line" ) ] }
        , { selectors = [ Css.Pseudo [ Css.Before ] (Css.Class NumberLineClass) ], descriptor = [ ( "content", "counter(line)" ), ( "color", "red" ) ] }
        , { selectors = [ Css.Class GradientClass ], descriptor = [ ( "background-clip", "content-box !important" ) ] }
        , { selectors = [ Css.Class Cell400px ], descriptor = [ ( "width", "400px" ), ( "vertical-align", "top" ) ] }
        , { selectors = [ Css.Class Cell800px ], descriptor = [ ( "width", "800px" ), ( "vertical-align", "top" ) ] }
        , { selectors = [ Css.Class TestGood ], descriptor = [ ( "background", "lightgreen" ) ] }
        , { selectors = [ Css.Class TestBad ], descriptor = [ ( "background", "red" ) ] }
        , { selectors = [ Css.Class DisplayNone ], descriptor = [ ( "display", "none" ) ] }
        , { selectors = [ Css.Class Colophon ], descriptor = [ ( "font-size", "25%" ) ] }
        , { selectors = [ Css.Class SolidBlackBorder ], descriptor = [ ( "border-style", "solid" ), ( "border-color", "black" ) ] }
        , { selectors = [ Css.Class SolidTransparentBorder ], descriptor = [ ( "border-style", "solid" ), ( "border-color", "transparent" ) ] }
        , { selectors = [ Css.Class OutlineBorder ], descriptor = [ ( "border", "solid" ), ( "border-width", "1px" ) ] }
        ]


useClass : Class -> Html.Attribute msg
useClass =
    stylesheet.class


type Id
    = MyId


type Class
    = NumberLineClass
    | GradientClass
    | Cell800px
    | Cell400px
    | TestGood
    | TestBad
    | DisplayNone
    | Colophon
    | SolidBlackBorder
    | SolidTransparentBorder
    | OutlineBorder
