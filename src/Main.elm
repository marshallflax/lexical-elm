module Main exposing (main)

import Controller
import DragView
import Html
import Types
import View


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = Controller.init
        , update = Controller.update
        , view = View.root
        , subscriptions =
            combineSubscriptions
                [ View.viewSubscriptions
                , Controller.webSubscriptions
                , DragView.dragSubscriptions
                ]
        }


combineSubscriptions : List (m -> Sub msg) -> (m -> Sub msg)
combineSubscriptions list model =
    Sub.batch (List.map ((|>) model) list)
