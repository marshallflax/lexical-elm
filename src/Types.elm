module Types exposing (..)

import Array exposing (Array)
import ColoredWord exposing (ColoredWord)
import FreqInfo exposing (FreqInfo)
import Keyboard exposing (..)
import Set exposing (Set)
import Table
import Testing exposing (..)


type alias Model =
    SavedModel
        { workingColor : String
        , words : Array ColoredWord
        , workingWord : Int
        , workingNormalized : Set String
        , hideColors : Set String
        , frequencies : FreqInfo
        , lastKeyCode : Keyboard.KeyCode
        , bowlingResults : List Testing.TestResult
        , tableState : Table.State
        }


type alias SavedModel a =
    { a
        | text : String
        , wordsPerLine : Int
    }


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
