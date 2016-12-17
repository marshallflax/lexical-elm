module Main exposing (main)

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
        , subscriptions = View.subscriptions
        }
