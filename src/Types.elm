module Types exposing (..)

import Set exposing (Set)
import Array exposing (Array)


type alias ColoredWord =
    { colors : Set String
    , text : String
    , normalized : String
    }


type alias Model =
    { text : String
    , workingColor : String
    , words : Array ColoredWord
    , workingWord : Int
    , hideColors : Set String
    , wordsPerLine : Int
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
