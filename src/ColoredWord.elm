module ColoredWord exposing (..)

import Array exposing (Array)
import Regex
import Set exposing (Set)


type alias ColoredWord =
    { colors : Set String
    , text : String
    , normalized : String
    }


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    let
        chunkArray : Array String
        chunkArray =
            Array.fromList (Regex.split Regex.All (Regex.regex "\\s+") input)
    in
        Array.map chunkToColoredword chunkArray


nonMaybeColoredWord : Maybe ColoredWord -> ColoredWord
nonMaybeColoredWord =
    Maybe.withDefault { text = "", colors = Set.empty, normalized = "" }


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


normalize : String -> String
normalize text =
    Regex.replace Regex.All
        (Regex.regex "[^a-z0-9]")
        (\_ -> "")
        (String.toLower text)


dumpColoredWord : ColoredWord -> String
dumpColoredWord cw =
    if (Set.isEmpty cw.colors) then
        cw.text
    else
        cw.text ++ "<" ++ (String.join "," (Set.toList cw.colors)) ++ ">"


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
