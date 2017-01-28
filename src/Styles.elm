module Styles exposing (..)

import Css
import Html


stylesheet : Css.Stylesheet Id Class msg
stylesheet =
    Css.stylesheet imports rules


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


imports : List String
imports =
    []


rules : List { descriptor : Css.Descriptor, selectors : List (Css.Sel Id Class) }
rules =
    [ { selectors = [ Css.Class NumberLineClass ]
      , descriptor = [ ( "counter-increment", "line" ) ]
      }
    , { selectors = [ Css.Pseudo [ Css.Before ] (Css.Class NumberLineClass) ]
      , descriptor = [ ( "content", "counter(line)" ), ( "color", "red" ) ]
      }
    , { selectors = [ Css.Class GradientClass ]
      , descriptor = [ ( "background-clip", "content-box !important" ) ]
      }
    , { selectors = [ Css.Class Cell400px ]
      , descriptor = [ ( "width", "400px" ), ( "vertical-align", "top" ) ]
      }
    , { selectors = [ Css.Class Cell800px ]
      , descriptor = [ ( "width", "800px" ), ( "vertical-align", "top" ) ]
      }
    ]
