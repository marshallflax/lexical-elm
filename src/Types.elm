module Types exposing (..)

import Array exposing (Array)
import ColoredWord exposing (ColoredWord)
import FreqInfo exposing (FreqInfo)
import Set exposing (Set)


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    , workingWord : Int
    , workingNormalized : Set String
    , hideColors : Set String
    , wordsPerLine : Int
    , frequencies : FreqInfo
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
