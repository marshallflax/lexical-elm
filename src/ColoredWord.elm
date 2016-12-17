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


empty : ColoredWord
empty =
    { text = "", colors = Set.empty, normalized = "" }


tokenize : String -> List String
tokenize input =
    let
        nonword : String
        nonword =
            " \\f\\n\\r\\t\\v\\u00a0\\u1680\\u180e\\u2000-\\u200a\\u2028\\u2029\\u202f\\u205f\\u3000\\ufeff"

        pattern : String
        pattern =
            "[^" ++ nonword ++ "]+[" ++ nonword ++ "]*"
    in
        Regex.find Regex.All (Regex.regex pattern) input
            |> List.map .match


splitIntoColorwords : String -> Array ColoredWord
splitIntoColorwords input =
    tokenize input
        |> Array.fromList
        |> Array.map chunkToColoredword


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
        (Regex.regex "[^_a-z0-9'’]")
        -- both apostrophe and single-quote!
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
    List.filterMap
        (\cw ->
            if (Set.member specifiedColor cw.colors) then
                Just cw.text
            else
                Nothing
        )
        (Array.toList coloredWordList)
