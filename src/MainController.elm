module MainController exposing (init)

import BowlingScoreTest
import BowlingScoreView
import Char
import Dict
import LexicalController
import Mouse exposing (Position)
import Types exposing (..)


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
