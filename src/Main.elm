module Main exposing (main)

import Controller
import DragView
import Html
import MainController
import MainView
import Misc
import Types


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = MainController.init
        , update = Controller.update
        , view = MainView.root
        , subscriptions =
            Misc.combineSubscriptions
                [ MainView.viewSubscriptions
                , Controller.webSubscriptions
                , DragView.dragSubscriptions
                ]
        }
