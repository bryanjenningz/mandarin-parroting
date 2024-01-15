module Dictionary exposing (Model, Msg, fetch, init, search, update, view)

import Array exposing (Array)
import Html exposing (Html, article, div, h2, p, text)
import Html.Attributes exposing (class)
import Http
import Parser exposing ((|.), (|=), Parser)



-- MODEL


type Model
    = EmptyDictionary
    | Dictionary DictionaryData
    | FailedToLoadDictionary


type alias DictionaryData =
    { traditional : Array String
    , simplified : Array String
    }


init : Model
init =
    EmptyDictionary


fetch : (Msg -> msg) -> Cmd msg
fetch toMsg =
    Http.get
        { url = "/dictionary.txt"
        , expect = Http.expectString (toMsg << LoadDictionary)
        }



-- UPDATE


type Msg
    = LoadDictionary (Result Http.Error String)


update : Msg -> Model -> Model
update msg _ =
    case msg of
        LoadDictionary (Err _) ->
            FailedToLoadDictionary

        LoadDictionary (Ok dictionary) ->
            let
                traditional =
                    dictionary
                        |> String.split "\n"
                        |> List.filter (\line -> not (String.startsWith "#" line))

                simplified =
                    traditional
                        |> List.sortBy toSimplified
            in
            Dictionary
                { traditional = Array.fromList traditional
                , simplified = Array.fromList simplified
                }



-- SEARCH


search : String -> DictionaryData -> Maybe Line
search searchText dictionaryData =
    if String.isEmpty searchText then
        Nothing

    else
        case binarySearchTraditional searchText dictionaryData of
            Nothing ->
                case binarySearchSimplified searchText dictionaryData of
                    Nothing ->
                        search (String.dropRight 1 searchText) dictionaryData

                    Just line ->
                        Just line

            Just line ->
                Just line



-- VIEW


view : String -> Model -> Html msg
view searchText model =
    div [ class "bg-black text-white z-10" ]
        [ case model of
            EmptyDictionary ->
                text "Waiting for dictionary to load..."

            FailedToLoadDictionary ->
                text "Failed to load dictionary."

            Dictionary dictionaryData ->
                case search searchText dictionaryData of
                    Nothing ->
                        text ("No results for the text \"" ++ searchText ++ "\"")

                    Just line ->
                        viewLine line
        ]



-- INTERNAL


binarySearchTraditional : String -> DictionaryData -> Maybe Line
binarySearchTraditional searchText dictionaryData =
    binarySearch
        { lower = 0
        , upper = Array.length dictionaryData.traditional
        , compareBy = toTraditional
        , searchText = searchText
        , dictionary = dictionaryData.traditional
        }
        |> Maybe.andThen (\i -> Array.get i dictionaryData.traditional)
        |> Maybe.andThen parseLine


binarySearchSimplified : String -> DictionaryData -> Maybe Line
binarySearchSimplified searchText dictionaryData =
    binarySearch
        { lower = 0
        , upper = Array.length dictionaryData.simplified
        , compareBy = toSimplified
        , searchText = searchText
        , dictionary = dictionaryData.simplified
        }
        |> Maybe.andThen (\i -> Array.get i dictionaryData.simplified)
        |> Maybe.andThen parseLine


type alias BinarySearchProps =
    { lower : Int
    , upper : Int
    , compareBy : String -> String
    , searchText : String
    , dictionary : Array String
    }


binarySearch : BinarySearchProps -> Maybe Int
binarySearch props =
    if props.lower > props.upper then
        Nothing

    else
        let
            mid =
                (props.lower + props.upper) // 2

            midText =
                props.dictionary
                    |> Array.get mid
                    |> Maybe.map props.compareBy
                    |> Maybe.withDefault ""
        in
        if props.searchText == midText then
            Just mid

        else if props.searchText > midText then
            binarySearch { props | lower = mid + 1 }

        else
            binarySearch { props | upper = mid - 1 }


toTraditional : String -> String
toTraditional line =
    line
        |> parseLine
        |> Maybe.map .traditional
        |> Maybe.withDefault ""


toSimplified : String -> String
toSimplified line =
    line
        |> parseLine
        |> Maybe.map .simplified
        |> Maybe.withDefault ""



-- LINE


type alias Line =
    { simplified : String
    , traditional : String
    , pinyin : String
    , definitions : List String
    }


parseLine : String -> Maybe Line
parseLine line =
    Parser.run lineParser line
        |> Result.toMaybe


lineParser : Parser Line
lineParser =
    Parser.succeed
        (\traditional simplified pinyin definitions ->
            { traditional = traditional
            , simplified = simplified
            , pinyin = pinyin
            , definitions = definitions |> String.dropRight 1 |> String.split "/"
            }
        )
        |= Parser.getChompedString (Parser.chompUntil " ")
        |. Parser.symbol " "
        |= Parser.getChompedString (Parser.chompUntil " ")
        |. Parser.symbol " ["
        |= Parser.getChompedString (Parser.chompUntil "]")
        |. Parser.symbol "] /"
        |= Parser.getChompedString (Parser.chompUntilEndOr "\n")


viewLine : Line -> Html msg
viewLine line =
    article []
        [ h2 [ class "text-2xl flex gap-3" ]
            [ div [] [ text line.traditional ]
            , if line.traditional /= line.simplified then
                div [] [ text line.simplified ]

              else
                text ""
            ]
        , div [ class "text-lg" ] [ text line.pinyin ]
        , p [ class "text-lg" ] [ text (String.join "; " line.definitions) ]
        ]
