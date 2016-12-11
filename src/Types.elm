module Types exposing (..)

import Array exposing (Array)
import ColoredWord exposing (..)
import Set exposing (Set)
import NGram exposing (..)


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    , workingWord : Int
    , hideColors : Set String
    , wordsPerLine : Int
    , frequencies : FreqInfo String
    }


type Msg
    = SetText String
    | SetCurrentColor String
    | ToggleColor Int String
    | SetCurrentWord Int
    | ToggleColorEnabled String
    | EnableAllColors
    | HideSomeColors (List String)
    | ResetSomeColors (List String)
    | SetWordsPerLine String
