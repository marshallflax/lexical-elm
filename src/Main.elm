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
        , subscriptions = myBatch [ View.viewSubscriptions, State.webSubscriptions ]
        }


myBatch : List (d -> Sub msg) -> (d -> Sub msg)
myBatch list model =
    Sub.batch (mapWithConstant model list)


mapWithConstant : a -> List (a -> b) -> List b
mapWithConstant constantA =
    List.map ((|>) constantA)
