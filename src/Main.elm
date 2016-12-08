module Main exposing (..)

import Html
import Types exposing (..)
import State exposing (..)
import View exposing (root)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none ), view = root, update = myUpdate, subscriptions = subscriptions }
