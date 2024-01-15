module Dictionary exposing (Model, Msg, fetch, init, search, update, view)

import Array exposing (Array)
import Html exposing (Html, article, div, h2, p, text)
import Html.Attributes exposing (class)
import Http



-- MODEL


type Model
    = EmptyDictionary
    | Dictionary DictionaryData
    | FailedToLoadDictionary


type alias DictionaryData =
    { simplified : Array String
    , traditional : Array String
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
                simplified =
                    dictionary
                        |> String.split "\n"
                        |> List.filter (\line -> not (String.startsWith "#" line))

                traditional =
                    simplified |> List.sortBy toTraditional
            in
            Dictionary
                { simplified = Array.fromList simplified
                , traditional = Array.fromList traditional
                }



-- SEARCH


search : String -> DictionaryData -> Maybe Line
search searchText dictionaryData =
    if String.isEmpty searchText then
        Nothing

    else
        case binarySearchSimplified searchText dictionaryData of
            Nothing ->
                case binarySearchTraditional searchText dictionaryData of
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


toSimplified : String -> String
toSimplified line =
    line
        |> parseLine
        |> Maybe.map .simplified
        |> Maybe.withDefault ""


toTraditional : String -> String
toTraditional line =
    line
        |> parseLine
        |> Maybe.map .traditional
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
    case line |> String.split "\t" of
        [ simplified, traditional, pinyin, definitions ] ->
            Just
                { simplified = simplified
                , traditional = traditional
                , pinyin = pinyin
                , definitions = definitions |> String.split "; "
                }

        _ ->
            Nothing


viewLine : Line -> Html msg
viewLine line =
    article [ class "flex flex-col p-4 gap-4 border border-white rounded-lg bg-black text-white" ]
        [ h2 [] [ text (line.traditional ++ " " ++ line.simplified) ]
        , div [] [ text line.pinyin ]
        , p [] [ text (String.join "; " line.definitions) ]
        ]
