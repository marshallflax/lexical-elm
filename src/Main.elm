module Main exposing (main)

import Controller
import DragView
import Html
import MainController
import Misc
import Types
import View


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = MainController.init
        , update = Controller.update
        , view = View.root
        , subscriptions =
            Misc.combineSubscriptions
                [ View.viewSubscriptions
                , Controller.webSubscriptions
                , DragView.dragSubscriptions
                ]
        }
