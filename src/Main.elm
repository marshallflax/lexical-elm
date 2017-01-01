module Main exposing (main)

import Controller
import DragView
import Html
import Misc
import Types
import View


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = Controller.init
        , update = Controller.update
        , view = View.root
        , subscriptions =
            Misc.combineSubscriptions
                [ View.viewSubscriptions
                , Controller.webSubscriptions
                , DragView.dragSubscriptions
                ]
        }
