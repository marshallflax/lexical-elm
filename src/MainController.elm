module MainController exposing (..)

import Array exposing (Array)
import BowlingScoreTest
import BowlingScoreView
import Char exposing (..)
import Dict exposing (..)
import FreqInfo exposing (..)
import Mouse exposing (Position)
import Set exposing (Set)
import Types exposing (..)

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
