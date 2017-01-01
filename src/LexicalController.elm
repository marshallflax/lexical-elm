module LexicalController exposing (..)

import Set exposing (Set)
import Types exposing (..)


lexicalUpdate : LexicalCmd -> Model -> ( Model, Cmd Msg )
lexicalUpdate msg model =
    case msg of
        EnableAllColors ->
            ( { model | hideColors = Set.empty }, Cmd.none )

        HideSomeColors colorList ->
            ( { model | hideColors = Set.union model.hideColors (Set.fromList colorList) }, Cmd.none )

        ResetSomeColors colorList ->
            ( { model | hideColors = Set.diff model.hideColors (Set.fromList colorList) }, Cmd.none )

        SetCurrentColor newDefaultColor ->
            ( { model | workingColor = newDefaultColor }, Cmd.none )
