module Controller exposing (..)

import Dict exposing (..)
import DragController exposing (..)
import Json.Decode
import LexicalController exposing (..)
import Types exposing (..)
import WebSocket


echoServer : String
echoServer =
    "wss://echo.websocket.org"


webSubscriptions : Model -> Sub Msg
webSubscriptions model =
    WebSocket.listen echoServer WebsocketMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of


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

        WebsocketMessage msg ->
            case
                Json.Decode.decodeString Types.savedModelDecoder msg
            of
                Ok decodedModel ->
                    ( { model | wordsPerLine = decodedModel.wordsPerLine }
                        |> updateModelWithNewText ("Got: " ++ decodedModel.text)
                    , Cmd.none
                    )

                Err msg ->
                    ( model
                        |> updateModelWithNewText msg
                    , Cmd.none
                    )

        DragMessage key dragVerb ->
            ( { model
                | draggables = Dict.update key (Maybe.map <| DragController.do dragVerb) model.draggables
              }
            , Cmd.none
            )

        LexicalMessage cmd ->
            lexicalUpdate cmd model
