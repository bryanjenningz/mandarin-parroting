module Dictionary exposing (Model, Msg, init, search, update)

import Array exposing (Array)
import Http



-- MODEL


type Model
    = EmptyDictionary
    | Dictionary DictionaryData
    | FailedToLoadDictionary Http.Error


type alias DictionaryData =
    { simplified : Array String
    , traditional : Array String
    }


init : ( Model, Cmd Msg )
init =
    ( EmptyDictionary, fetch )


fetch : Cmd Msg
fetch =
    Http.get
        { url = "/dictionary.txt"
        , expect = Http.expectString LoadDictionary
        }



-- UPDATE


type Msg
    = LoadDictionary (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        LoadDictionary (Err error) ->
            ( FailedToLoadDictionary error, Cmd.none )

        LoadDictionary (Ok dictionary) ->
            let
                simplified =
                    dictionary |> String.split "\n"

                traditional =
                    simplified |> List.sortBy toTraditional
            in
            ( Dictionary
                { simplified = Array.fromList simplified
                , traditional = Array.fromList traditional
                }
            , Cmd.none
            )



-- SEARCH


search : String -> Model -> Maybe Line
search searchText model =
    case model of
        EmptyDictionary ->
            Nothing

        FailedToLoadDictionary _ ->
            Nothing

        Dictionary dictionaryData ->
            case binarySearchSimplified searchText dictionaryData of
                Nothing ->
                    binarySearchTraditional searchText dictionaryData

                Just line ->
                    Just line



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
