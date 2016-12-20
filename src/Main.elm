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
        , subscriptions = myLift [ View.viewSubscriptions, State.webSubscriptions ]
        }


myLift : List (m -> Sub Types.Msg) -> m -> Sub Types.Msg
myLift list model =
    Sub.batch (List.map (\def -> def model) list)
