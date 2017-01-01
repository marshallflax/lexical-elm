module LexicalController exposing (..)

import Array exposing (Array)
import ColoredWord exposing (..)
import FreqInfo exposing (..)
import List.Split
import Misc
import Regex exposing (..)
import Set exposing (Set)
import Types exposing (..)


lexicalUpdate : LexicalCmd -> Model -> ( Model, Cmd Msg )
lexicalUpdate msg model =
    case msg of
        SetText newText ->
            ( updateModelWithNewText newText model, Cmd.none )

        SetWordsPerLine wordString ->
            ( setWordsPerLine wordString model, Cmd.none )

        SetCurrentNormalized text ->
            ( { model
                | workingWord = -1
                , workingNormalized =
                    -- include full text in split so we know to show 2-grams cheaply
                    Set.insert text (Set.fromList (Regex.split Regex.All (Regex.regex "_") text))
              }
            , Cmd.none
            )

        EnableAllColors ->
            ( { model | hideColors = Set.empty }, Cmd.none )

        HideSomeColors colorList ->
            ( { model | hideColors = Set.union model.hideColors (Set.fromList colorList) }, Cmd.none )

        ResetSomeColors colorList ->
            ( { model | hideColors = Set.diff model.hideColors (Set.fromList colorList) }, Cmd.none )

        SetCurrentColor newDefaultColor ->
            ( { model | workingColor = newDefaultColor }, Cmd.none )

        SetCurrentWord index ->
            ( { model
                | workingWord = index
                , workingNormalized = Set.insert (currentWordFromIndex index model).normalized Set.empty
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


currentWordFromIndex : Int -> Model -> ColoredWord
currentWordFromIndex index model =
    Array.get index model.words
        |> Maybe.withDefault ColoredWord.empty


countWords : Model -> Int
countWords model =
    Array.length model.words


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


rainbowList : List (List String)
rainbowList =
    [ [ "Aqua", "Blue", "Green", "DarkTurquoise", "Fuchsia", "Lime", "Plum", "Yellow" ], [ "Beige", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ] ]


setWordsPerLine : String -> Model -> Model
setWordsPerLine wordString model =
    case
        String.toInt wordString
    of
        Err msg ->
            model

        Ok val ->
            { model | wordsPerLine = val }


dumpState : Model -> String
dumpState model =
    List.map dumpColoredWord (Array.toList model.words)
        |> (String.join " ")


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
