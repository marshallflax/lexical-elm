module Styles exposing (..)

import Css


stylesheet : Css.Stylesheet Id Class msg
stylesheet =
    Css.stylesheet imports rules


type Id
    = MyId


type Class
    = NumberLineClass
    | GradientClass


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
    ]
