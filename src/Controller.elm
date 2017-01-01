module Controller exposing (..)

import Array exposing (Array)
import BowlingScoreTest
import BowlingScoreView
import Char exposing (..)
import ColoredWord exposing (..)
import Dict exposing (..)
import DragController exposing (..)
import FreqInfo exposing (..)
import Json.Decode
import List.Split
import Misc
import Mouse exposing (Position)
import Regex exposing (..)
import Set exposing (Set)
import Types exposing (..)
import WebSocket


echoServer : String
echoServer =
    "wss://echo.websocket.org"


webSubscriptions : Model -> Sub Msg
webSubscriptions model =
    WebSocket.listen echoServer WebsocketMessage


rainbowList : List (List String)
rainbowList =
    [ [ "Aqua", "Blue", "Green", "DarkTurquoise", "Fuchsia", "Lime", "Plum", "Yellow" ], [ "Beige", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ] ]


init : ( Model, Cmd msg )
init =
    ( { text = "Hello"
      , workingColor = ""
      , words = Array.fromList []
      , workingWord = -1
      , workingNormalized = Set.empty
      , hideColors = Set.empty
      , wordsPerLine = 10
      , frequencies = FreqInfo.empty
      , lastKeyCode = Char.toCode '!'
      , bowlingResults = BowlingScoreTest.testResults
      , tableState = BowlingScoreView.initialTableState
      , draggables =
            Dict.empty
                |> Dict.insert "text1" (Draggable (Position 200 200) Nothing "Text1" "100" "100")
                |> Dict.insert "text2" (Draggable (Position 300 300) Nothing "Text2" "100" "100")
      }
    , Cmd.none
    )


updateModelWithNewText : String -> Model -> Model
updateModelWithNewText newText model =
    let
        words =
            splitIntoColorwords newText
    in
        { model
            | text = newText
            , words = words
            , frequencies = countFreq (Array.map .normalized words)
        }


setWordsPerLine : String -> Model -> Model
setWordsPerLine wordString model =
    case
        String.toInt wordString
    of
        Err msg ->
            model

        Ok val ->
            { model | wordsPerLine = val }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText newText ->
            ( updateModelWithNewText newText model, Cmd.none )

        SetCurrentColor newDefaultColor ->
            ( { model | workingColor = newDefaultColor }, Cmd.none )

        SetCurrentWord index ->
            ( { model
                | workingWord = index
                , workingNormalized = Set.insert (currentWordFromIndex index model).normalized Set.empty
              }
            , Cmd.none
            )

        SetCurrentNormalized text ->
            ( { model
                | workingWord = -1
                , workingNormalized =
                    -- include full text in split so we know to show 2-grams cheaply
                    Set.insert text (Set.fromList (Regex.split Regex.All (Regex.regex "_") text))
              }
            , Cmd.none
            )

        ToggleColorEnabled color ->
            ( { model | hideColors = Misc.toggleSet color model.hideColors }, Cmd.none )

        ToggleColor which newColor ->
            ( if (String.length newColor == 0) then
                model
              else
                let
                    currentColoredWord =
                        Array.get which model.words
                            |> Maybe.withDefault ColoredWord.empty

                    modifiedColoredWord =
                        { currentColoredWord | colors = Misc.toggleSet newColor currentColoredWord.colors }
                in
                    { model | words = Array.set which modifiedColoredWord model.words }
            , Cmd.none
            )

        EnableAllColors ->
            ( { model | hideColors = Set.empty }, Cmd.none )

        HideSomeColors colorList ->
            ( { model | hideColors = Set.union model.hideColors (Set.fromList colorList) }, Cmd.none )

        ResetSomeColors colorList ->
            ( { model | hideColors = Set.diff model.hideColors (Set.fromList colorList) }, Cmd.none )

        SetWordsPerLine wordString ->
            ( setWordsPerLine wordString model, Cmd.none )

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

        Drag key dragVerb xy ->
            let
                newDraggables : Dict String Draggable
                newDraggables =
                    Dict.update key (Maybe.map (DragController.do dragVerb xy)) model.draggables
            in
                ( { model | draggables = newDraggables }, Cmd.none )


countWords : Model -> Int
countWords model =
    Array.length model.words


currentWordFromIndex : Int -> Model -> ColoredWord
currentWordFromIndex index model =
    Array.get index model.words
        |> Maybe.withDefault ColoredWord.empty


dumpState : Model -> String
dumpState model =
    List.map dumpColoredWord (Array.toList model.words)
        |> (String.join " ")


partitionedList : Model -> List (List ( Int, ColoredWord ))
partitionedList model =
    (Array.toIndexedList model.words)
        |> List.Split.chunksOfLeft model.wordsPerLine


countWordsMatching : Model -> Int
countWordsMatching model =
    let
        matches coloredWord =
            Set.member coloredWord.normalized model.workingNormalized
    in
        Array.filter matches model.words |> Array.length
