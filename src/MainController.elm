module MainController exposing (init, update, combinedSubscriptions)

import BowlingScoreTest
import BowlingScoreView
import Char
import Dict
import DragController
import DragView
import LexicalController
import MainView
import Misc
import Mouse exposing (Position)
import Types exposing (Model, Msg(..), DraggableWidget, LexicalCmd(..), encodeSavedModel)
import WebSocket


combinedSubscriptions : Model -> Sub Msg
combinedSubscriptions =
    Misc.combineSubscriptions
        [ MainView.viewSubscriptions
        , webSubscriptions
        , DragView.dragSubscriptions
        ]


init : ( Model, Cmd msg )
init =
    ( { lexical = LexicalController.init
      , draggables =
            Dict.empty
                |> Dict.insert "text1" (DraggableWidget (Position 200 200) Nothing)
                |> Dict.insert "text2" (DraggableWidget (Position 300 300) Nothing)
      , tableState = BowlingScoreView.initialTableState
      , lastKeyCode = Char.toCode '!'
      , bowlingResults = BowlingScoreTest.testResults
      }
    , Cmd.none
    )


echoServer : String
echoServer =
    "wss://echo.websocket.org"


webSubscriptions : Model -> Sub Msg
webSubscriptions model =
    WebSocket.listen echoServer (LexicalMessage << WebsocketMessage)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LexicalMessage cmd ->
            let
                ( newLexical, cmds ) =
                    LexicalController.lexicalUpdate cmd model.lexical
            in
                ( { model | lexical = newLexical }, cmds )

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
                | draggables = Dict.update key (DragController.doCmd dragVerb |> Maybe.map) model.draggables
              }
            , Cmd.none
            )
