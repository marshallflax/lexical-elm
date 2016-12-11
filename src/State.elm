module State exposing (..)

import Array exposing (Array)
import ColoredWord exposing (..)
import Dict exposing (..)
import List.Split
import NGram exposing (..)
import Set exposing (Set)
import Types exposing (..)


rainbowList : List (List String)
rainbowList =
    [ [ "Aqua", "Blue", "Green", "DarkTurquoise", "Fuschia", "Lime", "Plum" ], [ "Beige", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : ( Model, Cmd msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { text = "Hello"
    , workingColor = ""
    , words = Array.fromList []
    , workingWord = -1
    , hideColors = Set.empty
    , wordsPerLine = 10
    , frequencies = Dict.empty
    }


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
                    , frequencies = countFreq (Array.map .text words)
                }
            , Cmd.none
            )

        SetCurrentColor newDefaultColor ->
            ( { model | workingColor = newDefaultColor }, Cmd.none )

        SetCurrentWord index ->
            ( { model | workingWord = index }, Cmd.none )

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


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        (Set.remove element set)
    else
        (Set.insert element set)


countWords : Model -> Int
countWords model =
    Array.length model.words


currentWordFromIndex : Model -> ColoredWord
currentWordFromIndex model =
    nonMaybeColoredWord (Array.get model.workingWord model.words)


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
        desired =
            (currentWordFromIndex model).normalized
    in
        Array.length (Array.filter (\cw -> (cw.normalized == desired)) model.words)
