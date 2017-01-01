module Controller exposing (..)

import Dict exposing (..)
import DragController exposing (..)
import LexicalController
import Types exposing (..)
import WebSocket


echoServer : String
echoServer =
    "wss://echo.websocket.org"


webSubscriptions : Model -> Sub Msg
webSubscriptions model =
    WebSocket.listen echoServer (\msg -> LexicalMessage (WebsocketMessage msg))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LexicalMessage cmd ->
            let
                ( newLexical, cmds ) =
                    LexicalController.lexicalUpdate cmd model.lexical
            in
                ( { model | lexical = newLexical }, Cmd.none )

        KeyMsg code ->
            ( { model | lastKeyCode = code }, Cmd.none )

        SetTableState tableState ->
            ( { model | tableState = tableState }, Cmd.none )

        SaveModel ->
            let
                encoded : String
                encoded =
                    Debug.log "serialized" (encodeSavedModel model)
            in
                ( model, WebSocket.send echoServer encoded )

        DragMessage key dragVerb ->
            ( { model
                | draggables = Dict.update key (Maybe.map <| DragController.do dragVerb) model.draggables
              }
            , Cmd.none
            )
