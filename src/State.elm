module State exposing (..)

import Array exposing (Array)
import Regex exposing (Regex, Match)
import Types exposing (..)
import Set exposing (Set)


rainbowList : List (List String)
rainbowList =
    [ [ "Aqua", "Blue", "Green", "DarkTurquoise", "Fuschia", "Lime", "Plum" ], [ "Beige", "Indigo", "Purple", "Crimson", "Violet", "Coral", "Pink", "Gold" ] ]


nonMaybeColoredWord : Maybe ColoredWord -> ColoredWord
nonMaybeColoredWord =
    Maybe.withDefault { text = "", colors = Set.empty, normalized = "" }


model : Model
model =
    { text = "Hello"
    , workingColor = ""
    , words = Array.fromList []
    , workingWord = -1
    , hideColors = Set.empty
    , wordsPerLine = 10
    }


chunkToColoredword : String -> ColoredWord
chunkToColoredword str =
    let
        textAndColors : Maybe (List (Maybe String))
        textAndColors =
            Regex.find Regex.All (Regex.regex "^([^<>]+)<([^>]+)>\\s*$") str
                |> List.head
                |> Maybe.map .submatches

        theTextMap : String
        theTextMap =
            textAndColors
                |> Maybe.andThen List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault str

        theColors : Set String
        theColors =
            textAndColors
                |> Maybe.map (List.drop 1)
                |> Maybe.andThen List.head
                |> Maybe.withDefault Nothing
                |> Maybe.map (Regex.split Regex.All (Regex.regex ","))
                |> Maybe.map Set.fromList
                |> Maybe.withDefault Set.empty
    in
        { text = theTextMap
        , colors = theColors
        , normalized = normalize theTextMap
        }


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    let
        chunkArray : Array String
        chunkArray =
            Array.fromList (Regex.split Regex.All (Regex.regex "\\s+") input)
    in
        Array.map chunkToColoredword chunkArray


normalize : String -> String
normalize text =
    Regex.replace Regex.All
        (Regex.regex "[^a-z0-9]")
        (\_ -> "")
        (String.toLower text)


myUpdate : Msg -> Model -> ( Model, Cmd Msg )
myUpdate msg model =
    case msg of
        SetText newtext ->
            ( { model
                | text = newtext
                , words = splitIntoColorwords newtext
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
            ( (if (String.length newColor == 0) then
                model
               else
                let
                    currentColoredWord =
                        nonMaybeColoredWord (Array.get which model.words)

                    modifiedColoredWord =
                        { currentColoredWord | colors = toggleSet newColor currentColoredWord.colors }
                in
                    { model | words = Array.set which modifiedColoredWord model.words }
              )
            , Cmd.none
            )

        EnableAllColors ->
            ( ({ model | hideColors = Set.empty }), Cmd.none )

        HideSomeColors colorList ->
            ( ({ model | hideColors = Set.union model.hideColors (Set.fromList colorList) }), Cmd.none )

        ResetSomeColors colorList ->
            ( ({ model | hideColors = Set.diff model.hideColors (Set.fromList colorList) }), Cmd.none )

        SetWordsPerLine wordString ->
            ( (case
                String.toInt wordString
               of
                Err msg ->
                    model

                Ok val ->
                    { model | wordsPerLine = val }
              )
            , Cmd.none
            )


toggleSet : comparable1 -> Set comparable1 -> Set comparable1
toggleSet element set =
    if (Set.member element set) then
        (Set.remove element set)
    else
        (Set.insert element set)


countWordsMatching : Model -> Int
countWordsMatching model =
    let
        desired =
            (currentWordFromIndex model).normalized
    in
        Array.length (Array.filter (\cw -> (cw.normalized == desired)) model.words)


countWords : Model -> Int
countWords model =
    Array.length model.words


currentWordFromIndex : Model -> ColoredWord
currentWordFromIndex model =
    nonMaybeColoredWord (Array.get model.workingWord model.words)


dumpColoredWord : ColoredWord -> String
dumpColoredWord cw =
    if (Set.isEmpty cw.colors) then
        cw.text
    else
        cw.text ++ "<" ++ (String.join "," (Set.toList cw.colors)) ++ ">"


dumpState : Model -> String
dumpState model =
    List.map dumpColoredWord (Array.toList model.words)
        |> (String.join " ")


matchingWordsForColor : String -> Array ColoredWord -> List String
matchingWordsForColor color coloredWordList =
    List.filterMap
        (\cw ->
            if (Set.member color cw.colors) then
                Just cw.text
            else
                Nothing
        )
        (Array.toList coloredWordList)
