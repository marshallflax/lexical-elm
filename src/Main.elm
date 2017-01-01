module Main exposing (main)

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
        , update = MainController.update
        , view = MainView.root
        , subscriptions =
            Misc.combineSubscriptions
                [ MainView.viewSubscriptions
                , MainController.webSubscriptions
                , DragView.dragSubscriptions
                ]
        }
