module Types exposing (..)

import Array exposing (Array)
import ColoredWord exposing (ColoredWord)
import Dict exposing (Dict)
import FreqInfo exposing (FreqInfo)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode exposing (encode)
import Keyboard exposing (KeyCode)
import Mouse exposing (Position)
import Set exposing (Set)
import Table
import Testing exposing (TestResult)


type alias Model =
    { lexical : LexicalModel
    , draggables : Dict String Draggable
    , lastKeyCode : Keyboard.KeyCode
    , bowlingResults : List ( Int, Testing.TestResult )
    , tableState : Table.State
    }


type alias LexicalModel =
    ComputedModel SavedModel


type alias ComputedModel a =
    { a
        | workingColor : String
        , words : Array ColoredWord
        , workingWord : Int
        , workingNormalized : Set String
        , hideColors : Set String
        , frequencies : FreqInfo
    }


type alias Draggable =
    { position : Position
    , drag : Maybe DragState
    , text : String
    , width : String
    , height : String
    }


type alias DragState =
    { start : Position
    , current : Position
    }


type alias SavedModel =
    { text : String
    , wordsPerLine : Int
    }


encodeSavedModel : Model -> String
encodeSavedModel model =
    Json.Encode.encode 0
        (Json.Encode.object
            [ ( "text", Json.Encode.string model.lexical.text )
            , ( "wordsPerLine", Json.Encode.int model.lexical.wordsPerLine )
            ]
        )


savedModelDecoder : Json.Decode.Decoder SavedModel
savedModelDecoder =
    JDP.decode SavedModel
        |> JDP.required "text" Json.Decode.string
        |> JDP.required "wordsPerLine" Json.Decode.int


type DragCmd
    = DragStart Mouse.Position
    | DragEnd Mouse.Position
    | DragAt Mouse.Position


type LexicalCmd
    = EnableAllColors
    | HideSomeColors (List String)
    | ResetSomeColors (List String)
    | SetCurrentColor String
    | SetCurrentNormalized String
    | SetCurrentWord Int
    | SetText String
    | SetWordsPerLine String
    | ToggleColor Int String
    | ToggleColorEnabled String
    | WebsocketMessage String


type Msg
    = LexicalMessage LexicalCmd
    | KeyMsg Keyboard.KeyCode
    | SetTableState Table.State
    | SaveModel
    | DragMessage String DragCmd
