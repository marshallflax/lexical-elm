module Main exposing (main)

import DragView
import Html
import State
import Types
import View


main : Program Never Types.Model Types.Msg
main =
    Html.program
        { init = State.init
        , update = State.update
        , view = View.root
        , subscriptions =
            combineSubscriptions
                [ View.viewSubscriptions
                , State.webSubscriptions
                , DragView.dragSubscriptions
                ]
        }


combineSubscriptions : List (m -> Sub msg) -> (m -> Sub msg)
combineSubscriptions list model =
    Sub.batch (List.map ((|>) model) list)
