module ColoredWord exposing (..)

import Array exposing (Array)
import Regex
import Set exposing (Set)


type alias NormalizedWord a =
    { a | normalized : String }


type alias WithColors a =
    { a | colors : Set String }


type alias TextWord a =
    { a | text : String }


type alias ColoredWord =
    NormalizedWord (WithColors (TextWord {}))


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    (Regex.split Regex.All (Regex.regex "\\s+") input)
        |> Array.fromList
        |> Array.map chunkToColoredword


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

        text : String
        text =
            textAndColors
                |> Maybe.andThen List.head
                |> Maybe.withDefault Nothing
                |> Maybe.withDefault str

        colors : Set String
        colors =
            textAndColors
                |> Maybe.map (List.drop 1)
                |> Maybe.andThen List.head
                |> Maybe.withDefault Nothing
                |> Maybe.map (Regex.split Regex.All (Regex.regex ","))
                |> Maybe.map Set.fromList
                |> Maybe.withDefault Set.empty
    in
        { text = text
        , colors = colors
        , normalized = normalize text
        }


normalize : String -> String
normalize text =
    Regex.replace Regex.All
        (Regex.regex "[^a-z0-9]")
        (\_ -> "")
        (String.toLower text)


dumpColoredWord : ColoredWord -> String
dumpColoredWord { colors, text } =
    if (Set.isEmpty colors) then
        text
    else
        text ++ "<" ++ (String.join "," (Set.toList colors)) ++ ">"


matchingWordsForColor : String -> Array ColoredWord -> List String
matchingWordsForColor specifiedColor coloredWordList =
    (Array.toList coloredWordList)
        |> List.filterMap
            (\{ colors, text } ->
                if (Set.member specifiedColor colors) then
                    Just text
                else
                    Nothing
            )
