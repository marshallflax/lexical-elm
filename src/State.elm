module State exposing (..)

import Array exposing (Array)
import Char exposing (..)
import ColoredWord exposing (..)
import FreqInfo exposing (..)
import List.Split
import Regex exposing (..)
import Set exposing (Set)
import Types exposing (..)


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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText newtext ->
            ( let
                words =
                    splitIntoColorwords newtext
              in
                { model
                    | text = newtext
                    , words = words
                    , frequencies = countFreq (Array.map .normalized words)
                }
            , Cmd.none
            )

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
                | workingWord =
                    -1
                , workingNormalized =
                    -- include full text in split so we know to show 2-grams cheaply
                    Set.insert text (Set.fromList (Regex.split Regex.All (Regex.regex "_") text))
              }
            , Cmd.none
            )

        ToggleColorEnabled color ->
            ( { model | hideColors = toggleSet color model.hideColors }, Cmd.none )

        ToggleColor which newColor ->
            ( if (String.length newColor == 0) then
                model
              else
                let
                    currentColoredWord =
                        nonMaybeColoredWord (Array.get which model.words)

                    modifiedColoredWord =
                        { currentColoredWord | colors = toggleSet newColor currentColoredWord.colors }
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
            ( case
                String.toInt wordString
              of
                Err msg ->
                    model

                Ok val ->
                    { model | wordsPerLine = val }
            , Cmd.none
            )

        KeyMsg code ->
            ( { model | lastKeyCode = code }, Cmd.none )


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        (Set.remove element set)
    else
        (Set.insert element set)


countWords : Model -> Int
countWords model =
    Array.length model.words


currentWordFromIndex : Int -> Model -> ColoredWord
currentWordFromIndex index model =
    nonMaybeColoredWord (Array.get index model.words)


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
        Array.length
            (Array.filter matches model.words)
