module MainController exposing (init, update, combinedSubscriptions)

import BowlingScoreTest
import BowlingScoreView
import Char
import Dict
import DragController exposing (..)
import DragView
import LexicalController
import MainView
import Misc
import Mouse exposing (Position)
import Types exposing (..)
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
                |> Dict.insert "text1" (Draggable (Position 200 200) Nothing "Text1" "100" "100")
                |> Dict.insert "text2" (Draggable (Position 300 300) Nothing "Text2" "100" "100")
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
