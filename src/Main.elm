module Main exposing (main)

import Html exposing (program)
import State exposing (update, subscriptions, init)
import Types exposing (Model, Msg)
import View exposing (root)


main : Program Never Model Msg
main =
    program
        { init = init, view = root, update = update, subscriptions = subscriptions }
