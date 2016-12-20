module Types exposing (..)

import Array exposing (Array)
import ColoredWord exposing (ColoredWord)
import FreqInfo exposing (FreqInfo)
import Json.Decode.Pipeline exposing (required, decode)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Table
import Testing exposing (..)


type alias Model =
    ComputedModel SavedModel


type alias ComputedModel a =
    { a
        | workingColor : String
        , words : Array ColoredWord
        , workingWord : Int
        , workingNormalized : Set String
        , hideColors : Set String
        , frequencies : FreqInfo
        , lastKeyCode : Keyboard.KeyCode
        , bowlingResults : List Testing.TestResult
        , tableState : Table.State
    }


type alias SavedModel =
    { text : String
    , wordsPerLine : Int
    }


encodeSavedModel : Model -> String
encodeSavedModel model =
    Json.Encode.encode 0
        (Json.Encode.object
            [ ( "text", Json.Encode.string model.text )
            , ( "wordsPerLine", Json.Encode.int model.wordsPerLine )
            ]
        )


decodeSavedModel : String -> SavedModel
decodeSavedModel json =
    { text = "Decoded Text"
    , wordsPerLine = 9
    }


savedModelDecoder : Json.Decode.Decoder SavedModel
savedModelDecoder =
    decode SavedModel
        |> required "text" Json.Decode.string
        |> required "wordsPerLine" Json.Decode.int


type Msg
    = SetText String
    | SetCurrentColor String
    | ToggleColor Int String
    | SetCurrentWord Int
    | SetCurrentNormalized String
    | ToggleColorEnabled String
    | EnableAllColors
    | HideSomeColors (List String)
    | ResetSomeColors (List String)
    | SetWordsPerLine String
    | KeyMsg Keyboard.KeyCode
    | SetTableState Table.State
    | SaveModel
    | WebsocketMessage String
