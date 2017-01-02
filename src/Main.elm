module Main exposing (main)

import Html
import MainController
import MainView
import Types


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = MainController.init
        , update = MainController.update
        , view = MainView.root
        , subscriptions = MainController.combinedSubscriptions
        }
